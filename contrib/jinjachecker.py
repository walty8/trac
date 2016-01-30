#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright (C) 2016 Edgewall Software
# All rights reserved.
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at http://trac.edgewall.org/wiki/TracLicense.

"""This tool help diagnose basic flaws in Jinja2 templates.

It tries to present useful hints to the template developer, in
particular to help resolve nesting issues.

"""

def usage():
    """
Usage: python %s [options] TEMPLATES...

Where options can be:
 -j, --jinja-only    Only checks the jinja2 structure
 -h, --html-only     Only validates the HTML
 -q, --quiet         Don't show the filtered content, only the errors

If no flags are given, both checks are performed.

An alternative usage is to run the tool via make, i.e. `make jinja`.
This will run the tool on all .html files.
"""

import re
import sys

from pkg_resources import parse_version as pv
from collections import namedtuple
from os.path import abspath, dirname, join, normpath
from StringIO import StringIO


# Setup XHTML validation

etree = None

def setup_html():
    global etree
    try:
        from lxml import etree
    except ImportError:
        print("can't validate the XHTML parts in Jinja2 templates"
              " (no lxml installed)")

    if etree and pv(etree.__version__) < pv('2.0.0'):
        # 2.0.7 and 2.1.x are known to work.
        print("can't validate the XHTML parts in Jinja2 templates"
              " (lxml < 2.0, api incompatibility)")

    if etree:
        # Note: this code derived from trac/tests/functional (FIXME)

        class Resolver(etree.Resolver):
            # ./contrib/jinjachecker.py # <- we live here
            # ./trac/tests/functional/  # <- there are the DTDs
            contrib_dir = dirname(abspath(__file__))
            base_dir = normpath(join(contrib_dir, '../trac/tests/functional'))

            def resolve(self, system_url, public_id, context):
                filename = join(self.base_dir, system_url.split("/")[-1])
                return self.resolve_filename(filename, context)
        parser = etree.XMLParser(dtd_validation=True)
        parser.resolvers.add(Resolver())
        etree.set_default_parser(parser)
    return etree


# -- Common ----------------------------------------------------------------

def main(args):
    status = 0
    if args:
        # FIXME
        only = quiet = None
        if args[0] in ('-j', '--jinja-only'):
            only = 'jinja'
        elif args[0] in ('-h', '--html-only'):
            only = 'html'
        if only:
            del args[0]
        if args[0] in ('-q', '--quiet'):
            quiet = True
        from glob import glob
        setup_html()
        for arg in args:
            for template in glob(arg):
                status += analyze(template, only, quiet)
        if status > 0:
            print("One error found." if status == 1 else
                  "%d errors found." % status)
    else:
        print(usage())
    exit(status)


def analyze(jinja_template, only=None, quiet=False):
    """Analyzes a Jinja2 template, its control structure as well as the
    structure of the HTML.
    """
    with open(jinja_template, 'r') as f:
        lines = f.readlines()
    line_statements, html, html_hints = scan(lines)
    issues_j = issues_h = 0
    if only != 'html':
        issues_j = check_jinja(jinja_template, line_statements, quiet)
        report_errors('Jinja2', issues_j)
    if only != 'jinja' and etree:
        issues_h = check_html(jinja_template, html, html_hints, quiet)
        report_errors('XHTML', issues_h)
    return issues_j + issues_h


def report_errors(kind, issues):
    if issues:
        print('%s: %d errors' % (kind, issues))
    else:
        print('%s: OK' % kind)



# -- Jinja2 ----------------------------------------------------------------

# Jinja2 Syntax
#
# Note: keep in sync with trac/web/chrome.py

BLOCK_START_STRING = '{{'
BLOCK_END_STRING = '}}'

COMMENT_START_STRING = '{#'
COMMENT_END_STRING = '#}'

LINE_STATEMENT_PREFIX = '#'
LINE_COMMENT_PREFIX = '##'

JINJA2_BLOCK_KEYWORDS = ('block', 'call', 'for', 'if', 'macro', 'raw', 'with')


Statement = namedtuple('Statement',
                       ('linenum', 'indent', 'end', 'kw', 'expr', 'colon'))

LINE_STATEMENT_RE = re.compile(r'^(\s*)%s-?(\s*)(end)?(\w+)(.*?)?(:)?$' %
                               LINE_STATEMENT_PREFIX)

JINJACHECK_RE = re.compile(r'jinjacheck: "([^"]+)" OK')


def scan(lines):
    """Scans template lines and separate Jinja2 structure from HTML structure.
    """
    lines = iter(enumerate(lines))
    line_statements = []
    html = []
    html_hints = []
    def add_hint(linenum, comment_line):
        m = JINJACHECK_RE.search(sline)
        if m:
            html_hints.append((linenum, m.group(1)))
    try:
        while True:
            # pick a line
            linenum, line = lines.next()
            sline = line.strip()
            # skip empty lines
            if sline == '':
                html.append((linenum, '\n'))
                continue
            # skip comment blocks
            if sline.startswith(COMMENT_START_STRING):
                html.append((linenum, '\n'))
                add_hint(linenum, sline)
                while not sline.endswith(COMMENT_END_STRING):
                    html.append((linenum, '\n'))
                    add_hint(linenum, sline)
                    sline = lines.next()[1].strip()
                continue
            # skip line comments
            if sline.startswith(LINE_COMMENT_PREFIX):
                html.append((linenum, '\n'))
                add_hint(linenum, sline)
                continue
            # check for a line statement
            m = LINE_STATEMENT_RE.match(line)
            if m:
                html.append((linenum, '\n'))
                line_statements.append(
                    Statement(linenum, (len(m.group(1)) + len(m.group(2)) + 1),
                              m.group(3) or '', m.group(4),
                              m.group(5), m.group(6) or ''))
            else:
                html.append((linenum, line))
    except StopIteration:
        return line_statements, html, html_hints


def check_jinja(filename, line_statements, quiet):
    """Verifies proper nesting of Jinja2 control structures.
    """
    print("\n---- Jinja2 check on '%s' ------------------------\n" % filename)
    kw_stack = []
    issues = 0
    for s in line_statements:
        warn = []
        is_block = (s.kw in JINJA2_BLOCK_KEYWORDS or
                    s.kw == 'set' and '=' not in s.expr)
        if s.end:
            top = kw_stack and kw_stack[-1]
            if not is_block:
                warn.append("'end%s' is not a valid keyword" % s.kw)
            else:
                if top:
                    if s.kw == top.kw:
                        kw_stack.pop()
                    else:
                        warn.append(("'end%s' misplaced, current block is"
                                     " '%s' (at line %d)") %
                                    (s.kw, top.kw, top.linenum))
                else:
                    warn.append("'end%s' misplaced, not in a block" % s.kw)
            if s.expr:
                if s.kw == 'block':
                    if top and top.expr != s.expr:
                        warn.append(("'endblock %s' misplaced or misspelled,"
                                     " current block is 'block %s'") %
                                    (s.expr, top.expr))
                else:
                    warn.append("no expression allowed for 'end%s' statement"
                                % s.kw)
            if s.colon:
                warn.append("no ending colon wanted for 'end%s' statement"
                            % s.kw)
        else:
            if is_block:
                kw_stack.append(s)
            if s.expr == '' and s.kw not in ('with', 'else'):
                warn.append("expression missing in '%s' statement" % s.kw)
            if s.kw in ('block', 'extends', 'include', 'macro', 'set', 'with'):
                if s.colon:
                    warn.append("no ending colon wanted for '%s' statement"
                                % s.kw)
            elif not s.colon:
                warn.append("ending colon wanted for '%s' statement" % s.kw)
        issues += len(warn)
        print_statement(filename, s, warn, quiet)
    return issues


def print_statement(filename, s, warn=None, quiet=False):
    if not quiet:
        print('%5d %s %s%s%s%s' % (s.linenum + 1,
                                   ' ' * s.indent,
                                   '/' if s.end else '', s.kw.upper(),
                                   s.expr, s.colon))
    while warn:
        print('%s:%s: %s' % (filename, s.linenum + 1, warn.pop()))


# -- HTML ------------------------------------------------------------------

XHTML_DOCTYPE = '''<!DOCTYPE html \
    PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" \
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">'''


def check_html(filename, html_lines, html_hints, quiet):
    """Validates the given HTML (as XHTML actually)
    """
    global etree
    print("\n---- HTML check for '%s' -------------------------\n" % filename)
    # re-build the page content, replacing the DTD with the XHTML DTD,
    # or adding it if missing. Jinja2 expressions are removed.
    opened_braces = 0
    normalized_lines = []
    has_html_elt = has_head_elt = has_body_elt = False
    for linenum, line in html_lines:
        has_html_elt = has_html_elt or '<html>' in line
        has_head_elt = has_head_elt or '<head>' in line
        has_body_elt = has_body_elt or '<body>' in line
        if line.strip() != '<!DOCTYPE html>':
            normalized, opened_braces = remove_jinja_exprs(linenum, line,
                                                           opened_braces)
            normalized_lines.append(normalized)

    if not has_body_elt:
        normalized_lines[0] = '<body>' + normalized_lines[0]
        normalized_lines[-1] = normalized_lines[-1] + '</body>'
    if not has_head_elt:
        normalized_lines[0] = '<head><title/></head>' + normalized_lines[0]
    if not has_html_elt:
        normalized_lines[0] = '<html>' + normalized_lines[0]
        normalized_lines[-1] = normalized_lines[-1] + '</html>'
    normalized_lines[0] = XHTML_DOCTYPE + normalized_lines[0]
    page = '\n'.join(normalized_lines)
    ## print(page) # DEBUG
    etree.clear_error_log()
    try:
        # lxml will try to convert the URL to unicode by itself,
        # this won't work for non-ascii URLs, so help him
        etree.parse(StringIO(page), base_url='.') #  base_url ??
        return 0
    except etree.XMLSyntaxError as e:
        ignored = 0
        def get_hint(linenum):
            if html_hints:
                while True:
                    hint_linenum, hint = html_hints[0]
                    if hint_linenum >= linenum:
                        break
                    del html_hints[0]
                if hint_linenum == linenum:
                    if hint in msg:
                        del html_hints[0]
                        return hint
        errors = []
        for entry in e.error_log:
            errors.append((entry.line, entry.column, entry.message))
        for linenum, line in html_lines:
            if not quiet:
                print('%5d %s' % (linenum, line)),
            while errors and errors[0][0] == linenum:
                _, col, msg = errors[0]
                del errors[0]
                hint = get_hint(linenum)
                if hint:
                    ignored += 1
                print('%s:%s:%s: %s%s' %
                      (filename, linenum, col, msg,
                       ' (IGNORED "%s")' % hint if hint else ''))
        # in case some errors haven't been flushed at this point...
        for line, col, msg in errors:
            print('%s:%s:%s: %s' % (filename, line, col, msg))
        return len(e.error_log) - ignored


BRACES_RE = re.compile(r'(?:\b(id|for|selected|checked)=")?\$?([{}])')

def remove_jinja_exprs(linenum, line, opened_braces):
    """This probably could be a one-liner... ;-)
    """
    idx = 0
    line = line.replace('$', '')
    spans = []
    if opened_braces:
        spans.append([0, len(line), False])
    while True:
        m = BRACES_RE.search(line, idx)
        if m:
            idx = m.start(2)
            if line[idx] == '{':
                opened_braces += 1
                if opened_braces == 1:
                    spans.append([idx, len(line), m.group(1)])
            else:
                opened_braces -= 1
                if opened_braces == 0:
                    spans[-1][1] = idx
            idx += 1
        else:
            break
    normalized = ''
    pos = 0
    for start, end, attr in spans:
        if start > pos:
            normalized += line[pos:start]
        ## normalized += '@((%s))@' % line[start:end + 1] # DEBUG
        if attr in ('id', 'for'):
            normalized += "L%d-%d" % (linenum, start)
        elif attr in ('selected', 'checked'):
            normalized += attr
        pos = end + 1
    if pos < len(line):
        normalized += line[pos:-1]
    return normalized, opened_braces

if __name__ == '__main__':
    main(sys.argv[1:])
