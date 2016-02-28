# -*- coding: utf-8 -*-
#
# Copyright (C) 2003-2009 Edgewall Software
# All rights reserved.
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at http://trac.edgewall.org/wiki/TracLicense.
#
# This software consists of voluntary contributions made by many
# individuals. For the exact contribution history, see the revision
# history and logs, available at http://trac.edgewall.org/log/.

# Note that a significant part of the code below was inspired or
# copied from the Genshi project, the tag build API from
# genshi.builder and the HTMLSanitizer from genshi.filters.html.

from HTMLParser import HTMLParser
import re

from markupsafe import Markup, escape as escape_quotes

from genshi import HTML, unescape
from genshi.core import (Attrs, COMMENT, START, END, TEXT,
                         stripentities, striptags)
from genshi.input import ParseError
try:
    from babel.support import LazyProxy
except ImportError:
    LazyProxy = None

from trac.core import TracError
from trac.util.text import to_unicode

__all__ = ['Deuglifier', 'FormTokenInjector', 'TracHTMLSanitizer', 'escape',
           'find_element', 'html', 'plaintext', 'tag', 'to_fragment',
           'valid_html_bytes', 'unescape']

def escape(str, quotes=False):
    e = escape_quotes(str)
    if quotes or '&#3' not in e:
        return e
    return Markup(unicode(e).replace('&#34;', '"').replace('&#39;', "'"))


# -- Simplified genshi.builder API

class Fragment(object):
    """A fragment represents a sequence of strings or elements."""

    __slot__ = ('children')

    def __init__(self, *args):
        self.children = []
        for arg in args:
            self.append(arg)

    def __html__(self):
        return Markup(unicode(self))

    def __unicode__(self):
        return u''.join(escape(c) for c in self.children)

    def __add__(self, other):
        return Fragment(self, other)

    def append(self, arg):
        if arg: # ignore None, False, [], (), ''
            if isinstance(arg, (Fragment, basestring, int, float, long)):
                self.children.append(arg)
            else:
                # support iterators and generators
                try:
                    for elt in arg:
                        self.append(elt)
                except TypeError:
                    self.children.append(arg)
        elif arg is 0: # but not 0!
            self.children.append(u'0')

    def as_text(self):
        return u''.join(c.as_text() if isinstance(c, Fragment) else unicode(c)
                        for c in self.children)

    def __iter__(self):
        """Genshi compatibility layer. Will be removed in Trac 1.5.1."""
        yield TEXT, Markup(self), (None, -1, -1)


class Element(Fragment):
    """An element represents an HTML element, with a tag name, attributes
    and content.

    """

    VOID_ELEMENTS = set(('area', 'base', 'br', 'col', 'command', 'embed', 'hr',
                         'img', 'input', 'keygen', 'link', 'meta', 'param',
                         'source', 'track', 'wbr'))

    __slot__ = ('tag', 'attrib')

    attrib = {}

    def __init__(self, tag, *args, **kwargs):
        Fragment.__init__(self, *args)
        self.tag = unicode(tag)
        if kwargs:
            self.attrib = self._dict_from_kwargs(kwargs)

    def _dict_from_kwargs(self, kwargs):
        # TODO: share more logic with htmlattr_filter
        try:
            c = kwargs.pop('class_')
            kwargs['class'] = c
        except KeyError:
            pass
        return dict((k, escape_quotes(v)) for k, v in kwargs.iteritems()
                     if v is not None)

    def __call__(self, *args, **kwargs):
        if kwargs:
            d = self._dict_from_kwargs(kwargs)
            if d:
                if self.attrib:
                    self.attrib.update(d)
                else:
                    self.attrib = d
        for arg in args:
            self.append(arg)
        return self

    def __unicode__(self):
        elt = u'<' + self.tag
        if self.attrib:
            #elt += u''.join(' %s="%s"' % kv for kv in self.attrib.iteritems())
            # Note that sorting the attrs somehow reduces the impact on the
            # unit-tests, so we do that for now
            elt += u''.join(' %s="%s"' % (k, self.attrib[k])
                            for k in sorted(self.attrib.keys()))
        if self.children or self.tag not in self.VOID_ELEMENTS:
            elt += u'>' + Fragment.__unicode__(self) + u'</' + self.tag + u'>'
        else:
            elt += u' />'
        return elt

    def __html__(self):
        return Markup(unicode(self))


class ElementFactory(object):
    """A fragment factory can be used to build fragments and element of a
    given tag name.
    """
    __slot__ = ()

    def __call__(self, *args):
        return Fragment(*args)

    def __getattr__(self, tag):
        return Element(tag)

tag = html = ElementFactory()


class TracHTMLSanitizer(object):

    ## Jinja2: the last change upstream on HTMLSanitizer was 4 years
    ##         ago, and was the integration of some of the changes
    ##         below (r10788). It should be possible to grab the rest
    ##         and rewrite __call__ in terms of the regular
    ##         HTMLParser.

    """Sanitize HTML constructions which are potentially vector of
    phishing or XSS attacks, in user-supplied HTML.

    See also `genshi.HTMLSanitizer`_ from which the TracHTMLSanitizer
    has evolved.

    .. _genshi.HTMLSanitizer:
       http://genshi.edgewall.org/wiki/Documentation/filters.html#html-sanitizer

    """

    # TODO: check from time to time if there are any upstream changes
    #       we could integrate.

    SAFE_TAGS = frozenset(['a', 'abbr', 'acronym', 'address', 'area', 'b',
        'big', 'blockquote', 'br', 'button', 'caption', 'center', 'cite',
        'code', 'col', 'colgroup', 'dd', 'del', 'dfn', 'dir', 'div', 'dl', 'dt',
        'em', 'fieldset', 'font', 'form', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6',
        'hr', 'i', 'img', 'input', 'ins', 'kbd', 'label', 'legend', 'li', 'map',
        'menu', 'ol', 'optgroup', 'option', 'p', 'pre', 'q', 's', 'samp',
        'select', 'small', 'span', 'strike', 'strong', 'sub', 'sup', 'table',
        'tbody', 'td', 'textarea', 'tfoot', 'th', 'thead', 'tr', 'tt', 'u',
        'ul', 'var'])

    SAFE_ATTRS = frozenset(['abbr', 'accept', 'accept-charset', 'accesskey',
        'action', 'align', 'alt', 'axis', 'bgcolor', 'border', 'cellpadding',
        'cellspacing', 'char', 'charoff', 'charset', 'checked', 'cite', 'class',
        'clear', 'cols', 'colspan', 'color', 'compact', 'coords', 'datetime',
        'dir', 'disabled', 'enctype', 'for', 'frame', 'headers', 'height',
        'href', 'hreflang', 'hspace', 'id', 'ismap', 'label', 'lang',
        'longdesc', 'maxlength', 'media', 'method', 'multiple', 'name',
        'nohref', 'noshade', 'nowrap', 'prompt', 'readonly', 'rel', 'rev',
        'rows', 'rowspan', 'rules', 'scope', 'selected', 'shape', 'size',
        'span', 'src', 'start', 'style',
        'summary', 'tabindex', 'target', 'title',
        'type', 'usemap', 'valign', 'value', 'vspace', 'width'])

    SAFE_CSS = frozenset([
        # CSS 3 properties <http://www.w3.org/TR/CSS/#properties>
        'background', 'background-attachment', 'background-color',
        'background-image', 'background-position', 'background-repeat',
        'border', 'border-bottom', 'border-bottom-color',
        'border-bottom-style', 'border-bottom-left-radius',
        'border-bottom-right-radius', 'border-bottom-width',
        'border-collapse', 'border-color', 'border-left', 'border-left-color',
        'border-left-style', 'border-left-width', 'border-radius',
        'border-right', 'border-right-color', 'border-right-style',
        'border-right-width', 'border-spacing', 'border-style', 'border-top',
        'border-top-color', 'border-top-left-radius', 'border-top-right-radius',
        'border-top-style', 'border-top-width', 'border-width', 'bottom',
        'caption-side', 'clear', 'clip', 'color', 'content',
        'counter-increment', 'counter-reset', 'cursor', 'direction',
        'display', 'empty-cells', 'float', 'font', 'font-family', 'font-size',
        'font-style', 'font-variant', 'font-weight', 'height', 'left',
        'letter-spacing', 'line-height', 'list-style', 'list-style-image',
        'list-style-position', 'list-style-type', 'margin', 'margin-bottom',
        'margin-left', 'margin-right', 'margin-top', 'max-height', 'max-width',
        'min-height', 'min-width', 'opacity', 'orphans', 'outline',
        'outline-color', 'outline-style', 'outline-width', 'overflow',
        'padding', 'padding-bottom', 'padding-left', 'padding-right',
        'padding-top', 'page-break-after', 'page-break-before',
        'page-break-inside', 'position', 'quotes', 'right', 'table-layout',
        'text-align', 'text-decoration', 'text-indent', 'text-transform',
        'top', 'unicode-bidi', 'vertical-align', 'visibility', 'white-space',
        'widows', 'width', 'word-spacing', 'z-index',
    ])

    SAFE_SCHEMES = frozenset(['file', 'ftp', 'http', 'https', 'mailto', None])

    URI_ATTRS = frozenset(['action', 'background', 'dynsrc', 'href', 'lowsrc',
        'src'])

    def __init__(self, safe_schemes=SAFE_SCHEMES, safe_css=SAFE_CSS,
                 safe_tags=SAFE_TAGS, safe_attrs=SAFE_ATTRS,
                 uri_attrs=URI_ATTRS):
        """Note: safe_schemes and safe_css have to remain the first
        parameters, for backward-compatibility purpose.
        """
        self.safe_tags = safe_tags
        # The set of tag names that are considered safe.
        self.safe_attrs = safe_attrs
        # The set of attribute names that are considered safe.
        self.safe_css = safe_css
        # The set of CSS properties that are considered safe.
        self.uri_attrs = uri_attrs
        # The set of names of attributes that may contain URIs.
        self.safe_schemes = safe_schemes
        # The set of URI schemes that are considered safe.

    # IE6 <http://heideri.ch/jso/#80>
    _EXPRESSION_SEARCH = re.compile(
        u'[eE\uFF25\uFF45]'         # FULLWIDTH LATIN CAPITAL LETTER E
                                    # FULLWIDTH LATIN SMALL LETTER E
        u'[xX\uFF38\uFF58]'         # FULLWIDTH LATIN CAPITAL LETTER X
                                    # FULLWIDTH LATIN SMALL LETTER X
        u'[pP\uFF30\uFF50]'         # FULLWIDTH LATIN CAPITAL LETTER P
                                    # FULLWIDTH LATIN SMALL LETTER P
        u'[rR\u0280\uFF32\uFF52]'   # LATIN LETTER SMALL CAPITAL R
                                    # FULLWIDTH LATIN CAPITAL LETTER R
                                    # FULLWIDTH LATIN SMALL LETTER R
        u'[eE\uFF25\uFF45]'         # FULLWIDTH LATIN CAPITAL LETTER E
                                    # FULLWIDTH LATIN SMALL LETTER E
        u'[sS\uFF33\uFF53]{2}'      # FULLWIDTH LATIN CAPITAL LETTER S
                                    # FULLWIDTH LATIN SMALL LETTER S
        u'[iI\u026A\uFF29\uFF49]'   # LATIN LETTER SMALL CAPITAL I
                                    # FULLWIDTH LATIN CAPITAL LETTER I
                                    # FULLWIDTH LATIN SMALL LETTER I
        u'[oO\uFF2F\uFF4F]'         # FULLWIDTH LATIN CAPITAL LETTER O
                                    # FULLWIDTH LATIN SMALL LETTER O
        u'[nN\u0274\uFF2E\uFF4E]'   # LATIN LETTER SMALL CAPITAL N
                                    # FULLWIDTH LATIN CAPITAL LETTER N
                                    # FULLWIDTH LATIN SMALL LETTER N
    ).search

    # IE6 <http://openmya.hacker.jp/hasegawa/security/expression.txt>
    #     7) Particular bit of Unicode characters
    _URL_FINDITER = re.compile(
        u'[Uu][Rr\u0280][Ll\u029F]\s*\(([^)]+)').finditer

    def __call__(self, stream):
        """Apply the filter to the given stream.

        :param stream: the markup event stream to filter
        """
        waiting_for = None

        for kind, data, pos in stream:
            if kind is START:
                if waiting_for:
                    continue
                tag, attrs = data
                if not self.is_safe_elem(tag, attrs):
                    waiting_for = tag
                    continue

                new_attrs = self.sanitize_attrs(dict(attrs)).iteritems()

                yield kind, (tag, Attrs(new_attrs)), pos

            elif kind is END:
                tag = data
                if waiting_for:
                    if waiting_for == tag:
                        waiting_for = None
                else:
                    yield kind, data, pos

            elif kind is not COMMENT:
                if not waiting_for:
                    yield kind, data, pos

    def is_safe_css(self, prop, value):
        """Determine whether the given css property declaration is to be
        considered safe for inclusion in the output.
        """
        if prop not in self.safe_css:
            return False
        # Position can be used for phishing, 'static' excepted
        if prop == 'position':
            return value.lower() == 'static'
        # Negative margins can be used for phishing
        if prop.startswith('margin'):
            return '-' not in value
        return True

    def is_safe_elem(self, tag, attrs):
        """Determine whether the given element should be considered safe for
        inclusion in the output.

        :param tag: the tag name of the element
        :type tag: QName
        :param attrs: the element attributes
        :type attrs: Attrs
        :return: whether the element should be considered safe
        :rtype: bool
        """
        if tag not in self.safe_tags:
            return False
        if tag.localname == 'input':
            input_type = attrs.get('type', '').lower()
            if input_type == 'password':
                return False
        return True

    def is_safe_uri(self, uri):
        """Determine whether the given URI is to be considered safe for
        inclusion in the output.

        The default implementation checks whether the scheme of the URI is in
        the set of allowed URIs (`safe_schemes`).

        >>> sanitizer = HTMLSanitizer()
        >>> sanitizer.is_safe_uri('http://example.org/')
        True
        >>> sanitizer.is_safe_uri('javascript:alert(document.cookie)')
        False

        :param uri: the URI to check
        :return: `True` if the URI can be considered safe, `False` otherwise
        :rtype: `bool`
        """
        if '#' in uri:
            uri = uri.split('#', 1)[0] # Strip out the fragment identifier
        if ':' not in uri:
            return True # This is a relative URI
        chars = [char for char in uri.split(':', 1)[0] if char.isalnum()]
        return ''.join(chars).lower() in self.safe_schemes

    def sanitize_attrs(self, attrs):
        """Remove potentially dangerous attributes and sanitize
        the style attribute .

        :type attrs: dict corresponding to tag attributes
        :return: a dict containing only safe or sanitized attributes
        :rtype: dict
        """
        new_attrs = {}
        for attr, value in attrs.iteritems():
            value = stripentities(value)
            if attr not in self.safe_attrs:
                continue
            elif attr in self.uri_attrs:
                # Don't allow URI schemes such as "javascript:"
                if not self.is_safe_uri(value):
                    continue
            elif attr == 'style':
                # Remove dangerous CSS declarations from inline styles
                decls = self.sanitize_css(value)
                if not decls:
                    continue
                value = '; '.join(decls)
            new_attrs[attr] = value
        return new_attrs

    def sanitize_css(self, text):
        """Remove potentially dangerous property declarations from CSS code.

        In particular, properties using the CSS ``url()`` function with a scheme
        that is not considered safe are removed:

        >>> sanitizer = HTMLSanitizer()
        >>> sanitizer.sanitize_css(u'''
        ...   background: url(javascript:alert("foo"));
        ...   color: #000;
        ... ''')
        [u'color: #000']

        Also, the proprietary Internet Explorer function ``expression()`` is
        always stripped:

        >>> sanitizer.sanitize_css(u'''
        ...   background: #fff;
        ...   color: #000;
        ...   width: e/**/xpression(alert("foo"));
        ... ''')
        [u'background: #fff', u'color: #000']

        :param text: the CSS text; this is expected to be `unicode` and to not
                     contain any character or numeric references
        :return: a list of declarations that are considered safe
        :rtype: `list`
        """
        decls = []
        text = self._strip_css_comments(self._replace_unicode_escapes(text))
        for decl in filter(None, text.split(';')):
            decl = decl.strip()
            if not decl:
                continue
            try:
                prop, value = decl.split(':', 1)
            except ValueError:
                continue
            if not self.is_safe_css(prop.strip().lower(), value.strip()):
                continue
            is_evil = False
            if self._EXPRESSION_SEARCH(decl):
                is_evil = True
            for match in self._URL_FINDITER(decl):
                if not self.is_safe_uri(match.group(1)):
                    is_evil = True
                    break
            if not is_evil:
                decls.append(decl.strip())
        return decls

    _NORMALIZE_NEWLINES = re.compile(r'\r\n').sub
    _UNICODE_ESCAPE = re.compile(
        r"""\\([0-9a-fA-F]{1,6})\s?|\\([^\r\n\f0-9a-fA-F'"{};:()#*])""",
        re.UNICODE).sub

    def _replace_unicode_escapes(self, text):
        def _repl(match):
            t = match.group(1)
            if t:
                code = int(t, 16)
                chr = unichr(code)
                if code <= 0x1f:
                    # replace space character because IE ignores control
                    # characters
                    chr = ' '
                elif chr == '\\':
                    chr = r'\\'
                return chr
            t = match.group(2)
            if t == '\\':
                return r'\\'
            else:
                return t
        return self._UNICODE_ESCAPE(_repl,
                                    self._NORMALIZE_NEWLINES('\n', text))

    _CSS_COMMENTS = re.compile(r'/\*.*?\*/').sub

    def _strip_css_comments(self, text):
        """Replace comments with space character instead of superclass which
        removes comments to avoid problems when nested comments.
        """
        return self._CSS_COMMENTS(' ', text)


class Deuglifier(object):
    """Help base class used for cleaning up HTML riddled with ``<FONT
    COLOR=...>`` tags and replace them with appropriate ``<span
    class="...">``.

    The subclass must define a `rules()` static method returning a
    list of regular expression fragments, each defining a capture
    group in which the name will be reused for the span's class. Two
    special group names, ``font`` and ``endfont`` are used to emit
    ``<span>`` and ``</span>``, respectively.
    """
    def __new__(cls):
        self = object.__new__(cls)
        if not hasattr(cls, '_compiled_rules'):
            cls._compiled_rules = re.compile('(?:%s)' % '|'.join(cls.rules()))
        self._compiled_rules = cls._compiled_rules
        return self

    def format(self, indata):
        return re.sub(self._compiled_rules, self.replace, indata)

    def replace(self, fullmatch):
        for mtype, match in fullmatch.groupdict().items():
            if match:
                if mtype == 'font':
                    return '<span>'
                elif mtype == 'endfont':
                    return '</span>'
                return '<span class="code-%s">' % mtype


class HTMLTransform(HTMLParser):
    """Convenience base class for writing HTMLParsers.

    The default implementation of the HTMLParser ``handle_*`` methods
    do nothing, while in our case we try to rewrite the incoming
    document unmodified.

    """

    def __init__(self, out):
        HTMLParser.__init__(self)
        self.out = out

    def handle_starttag(self, tag, attrs):
        self.out.write(self.get_starttag_text())

    def handle_startendtag(self, tag, attrs):
        self.out.write(self.get_starttag_text())

    def handle_charref(self, name):
        self.out.write('&#%s;' % name)

    def handle_entityref(self, name):
        self.out.write('&%s;' % name)

    def handle_comment(self, data):
        self.out.write('<!--%s-->' % data)

    def handle_decl(self, data):
        self.out.write('<!%s>' % data)

    def handle_pi(self, data):
        self.out.write('<?%s?>' % data)

    def handle_data(self, data):
        self.out.write(data)

    def handle_endtag(self, tag):
        self.out.write('</' + tag + '>')


class FormTokenInjector(HTMLTransform):
    """Identify and protect forms from CSRF attacks.

    This filter works by adding a input type=hidden field to POST forms.
    """
    def __init__(self, form_token, out):
        HTMLTransform.__init__(self, out)
        self.token = form_token

    def handle_starttag(self, tag, attrs):
        HTMLTransform.handle_starttag(self, tag, attrs)
        if tag.lower() == 'form':
            for name, value in attrs:
                if name == 'method' and value.lower() == 'post':
                    self.out.write('<input type="hidden" name="__FORM_TOKEN"'
                                   ' value="%s"/>' % self.token)
                    break


def plaintext(text, keeplinebreaks=True):
    """Extract the text elements from (X)HTML content

    :param text: `unicode` or `Fragment`
    :param keeplinebreaks: optionally keep linebreaks
    """
    if isinstance(text, Fragment):
        text = text.as_text()
    else:
        text = stripentities(striptags(text))
    if not keeplinebreaks:
        text = text.replace(u'\n', u' ')
    return text


def find_element(frag, attr=None, cls=None, tag=None):
    """Return the first element in the fragment having the given attribute,
    class or tag, using a preorder depth-first search.
    """
    if isinstance(frag, Element):
        if attr is not None and attr in frag.attrib:
            return frag
        if cls is not None and cls in frag.attrib.get('class', '').split():
            return frag
        if tag is not None and tag == frag.tag:
            return frag
    if isinstance(frag, Fragment):
        for child in frag.children:
            elt = find_element(child, attr, cls, tag)
            if elt is not None:
                return elt

## Jinja2: not needed/wanted and also not used in the current Trac code base

def expand_markup(stream, ctxt=None):
    """A Genshi stream filter for expanding `genshi.Markup` events.

    Note: Expansion may not be possible if the fragment is badly
    formed, or partial.
    """
    for event in stream:
        if isinstance(event[1], Markup):
            try:
                for subevent in HTML(event[1]):
                    yield subevent
            except ParseError:
                yield event
        else:
            yield event


def to_fragment(input):
    """Convert input to a `Fragment` object."""

    while isinstance(input, Exception):
        input = input.args[0]
    if LazyProxy and isinstance(input, LazyProxy):
        input = input.value
    if isinstance(input, Fragment):
        return input
    return tag(to_unicode(input))


# Mappings for removal of control characters
_translate_nop = ''.join(chr(i) for i in range(256))
_invalid_control_chars = ''.join(chr(i) for i in range(32)
                                 if i not in [0x09, 0x0a, 0x0d])

def valid_html_bytes(bytes):
    return bytes.translate(_translate_nop, _invalid_control_chars)
