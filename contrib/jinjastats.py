#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright (C) 2016 Edgewall Software
# All rights reserved.
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at http://trac.edgewall.org/wiki/TracLicense.

"""This tool shows what remains to be converted to Jinja2 templates.

"""

def usage():
    return """
Usage: python %s [options]

Where options can be:
 -q, --quiet         Only show the overall completion %.

"""

from glob import glob
import sys


def main(args):
    quiet = None
    if args:
        if args[0] in ('-h', '--help'):
            print(usage())
            exit(0)
        if args[0] in ('-q', '--quiet'):
            quiet = True
    genshi = {}
    jinja2 = {}
    for template in glob('*/**/templates/*'):
        if template.endswith('~'):
            continue
        template = template.replace('\\', '/')
        lines = len(file(template).read().splitlines())
        dir, filename = template.rsplit('/', 1)
        if filename.startswith('j'):
            jinja2[dir + '/' + filename[1:]] = lines
        else:
            genshi[template] = lines
    converted = jconverted = total = 0
    for template, lines in sorted(genshi.iteritems()):
        total += lines
        if template in jinja2:
            converted += lines
            jconverted += jinja2[template]
        elif not quiet:
            print '%s (%d lines)' % (template, lines)
    print('%.2f%% converted to Jinja2'
          ' (%d Jinja2 lines corresponding to %d Genshi lines'
          ' on a total of %d Genshi lines)' %
          (100.0 * converted / total, jconverted, converted, total))
    exit(0)

if __name__ == '__main__':
    main(sys.argv[1:])
