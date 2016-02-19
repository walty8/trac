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

from HTMLParser import HTMLParser
import re

from markupsafe import Markup, escape as escape_quotes

from genshi import HTML, unescape
from genshi.core import stripentities, striptags, START, END
from genshi.filters.html import HTMLSanitizer
from genshi.input import ParseError
try:
    from babel.support import LazyProxy
except ImportError:
    LazyProxy = None

from trac.core import TracError
from trac.util.text import to_unicode

__all__ = ['Deuglifier', 'FormTokenInjector', 'TracHTMLSanitizer', 'escape',
           'find_element', 'html', 'plaintext', 'tag', 'to_fragment',
           'unescape']

def escape(str, quotes=False):
    return escape_quotes(str) # well, we just no longer care about quotes=False


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
        return u''.join(escape_quotes(c) for c in self.children)

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


class TracHTMLSanitizer(HTMLSanitizer):

    ## Jinja2: the last change upstream on HTMLSanitizer was 4 years
    ##         ago, and was the integration of some of the changes
    ##         below (r10788). It should be possible to grab the rest
    ##         and rewrite __call__ in terms of the regular
    ##         HTMLParser.

    """Sanitize HTML constructions which are potentially vector of
    phishing or XSS attacks, in user-supplied HTML.

    See also `genshi.HTMLSanitizer`_.

    .. _genshi.HTMLSanitizer:
       http://genshi.edgewall.org/wiki/Documentation/filters.html#html-sanitizer
    """

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

    def __init__(self, safe_schemes=HTMLSanitizer.SAFE_SCHEMES,
                 safe_css=SAFE_CSS):
        safe_attrs = HTMLSanitizer.SAFE_ATTRS | frozenset(['style'])
        safe_schemes = frozenset(safe_schemes)
        super(TracHTMLSanitizer, self).__init__(safe_attrs=safe_attrs,
                                                safe_schemes=safe_schemes)
        self.safe_css = frozenset(safe_css)

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

    def sanitize_attrs(self, attrs):
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

    def __call__(self, stream):
        """Remove input type="password" elements from the stream
        """
        suppress = False
        for kind, data, pos in super(TracHTMLSanitizer, self).__call__(stream):
            if kind is START:
                tag, attrs = data
                if (tag == 'input' and
                    attrs.get('type', '').lower() == 'password'):
                    suppress = True
                else:
                    yield kind, data, pos
            elif kind is END:
                if not suppress:
                    yield kind, data, pos
                suppress = False
            else:
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


class FormTokenInjector(HTMLParser):
    """Identify and protect forms from CSRF attacks.

    This filter works by adding a input type=hidden field to POST forms.
    """
    def __init__(self, form_token, out):
        HTMLParser.__init__(self)
        self.out = out
        self.token = form_token

    def handle_starttag(self, tag, attrs):
        self.out.write(self.get_starttag_text())
        if tag.lower() == 'form':
            for name, value in attrs:
                if name.lower() == 'method' and value.lower() == 'post':
                    self.out.write('<input type="hidden" name="__FORM_TOKEN"'
                                   ' value="%s"/>' % self.token)
                    break

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
