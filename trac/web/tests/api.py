# -*- coding: utf-8 -*-
#
# Copyright (C) 2005-2015 Edgewall Software
# All rights reserved.
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at http://trac.edgewall.org/wiki/TracLicense.
#
# This software consists of voluntary contributions made by many
# individuals. For the exact contribution history, see the revision
# history and logs, available at http://trac.edgewall.org/log/.

import os.path
import shutil
import sys
import tempfile
import unittest
from StringIO import StringIO

import trac.tests.compat
from genshi.builder import tag
from trac import perm
from trac.core import TracError
from trac.test import EnvironmentStub, Mock, MockPerm, locale_en
from trac.util import create_file
from trac.util.datefmt import utc
from trac.util.text import shorten_line
from trac.web.api import HTTPBadRequest, HTTPInternalError, Request, \
                         RequestDone, parse_arg_list
from tracopt.perm.authz_policy import AuthzPolicy


class RequestHandlerPermissionsTestCaseBase(unittest.TestCase):

    authz_policy = None

    def setUp(self, module_class):
        self.path = tempfile.mkdtemp(prefix='trac-')
        if self.authz_policy is not None:
            self.authz_file = os.path.join(self.path, 'authz_policy.conf')
            create_file(self.authz_file, self.authz_policy)
            self.env = EnvironmentStub(enable=['trac.*', AuthzPolicy],
                                       path=self.path)
            self.env.config.set('authz_policy', 'authz_file', self.authz_file)
            self.env.config.set('trac', 'permission_policies',
                                'AuthzPolicy, DefaultPermissionPolicy')
        else:
            self.env = EnvironmentStub(path=self.path)
        self.req_handler = module_class(self.env)

    def tearDown(self):
        self.env.reset_db_and_disk()

    def create_request(self, authname='anonymous', **kwargs):
        kw = {'perm': perm.PermissionCache(self.env, authname), 'args': {},
              'href': self.env.href, 'abs_href': self.env.abs_href,
              'tz': utc, 'locale': None, 'lc_time': locale_en,
              'session': Mock(get=lambda k, d=None: d,
                              set=lambda k, v, d=None: None),
              'authname': authname, 'chrome': {'notices': [], 'warnings': []},
              'method': None, 'get_header': lambda v: None, 'is_xhr': False}
        kw.update(kwargs)
        return Mock(**kw)

    def get_navigation_items(self, req):
        return self.req_handler.get_navigation_items(req)

    def grant_perm(self, username, *actions):
        permsys = perm.PermissionSystem(self.env)
        for action in actions:
            permsys.grant_permission(username, action)

    def process_request(self, req):
        self.assertTrue(self.req_handler.match_request(req))
        return self.req_handler.process_request(req)


def _make_environ(scheme='http', server_name='example.org',
                  server_port=80, method='GET', script_name='/trac',
                  **kwargs):
    environ = {'wsgi.url_scheme': scheme, 'wsgi.input': StringIO(''),
               'REQUEST_METHOD': method, 'SERVER_NAME': server_name,
               'SERVER_PORT': server_port, 'SCRIPT_NAME': script_name}
    environ.update(kwargs)
    return environ


def _make_req(environ, start_response, args={}, arg_list=(), authname='admin',
              form_token='A' * 40, chrome={'links': {}, 'scripts': []},
              perm=MockPerm(), session={}, tz=utc, locale=None, **kwargs):
    req = Request(environ, start_response)
    req.args = args
    req.arg_list = arg_list
    req.authname = authname
    req.form_token = form_token
    req.chrome = chrome
    req.perm = perm
    req.session = session
    req.tz = tz
    req.locale = locale
    for name, value in kwargs.iteritems():
        setattr(req, name, value)
    return req


class RequestTestCase(unittest.TestCase):

    def _make_environ(self, *args, **kwargs):
        return _make_environ(*args, **kwargs)

    def test_is_xhr_true(self):
        environ = self._make_environ(HTTP_X_REQUESTED_WITH='XMLHttpRequest')
        req = Request(environ, None)
        self.assertTrue(req.is_xhr)

    def test_is_xhr_false(self):
        environ = self._make_environ()
        req = Request(environ, None)
        self.assertFalse(req.is_xhr)

    def test_base_url(self):
        environ = self._make_environ()
        req = Request(environ, None)
        self.assertEqual('http://example.org/trac', req.base_url)

    def test_base_url_host(self):
        environ = self._make_environ(server_port=8080, HTTP_HOST='example.com')
        req = Request(environ, None)
        self.assertEqual('http://example.com/trac', req.base_url)

    def test_base_url_nondefaultport(self):
        environ = self._make_environ(server_port=8080)
        req = Request(environ, None)
        self.assertEqual('http://example.org:8080/trac', req.base_url)

    def test_base_url_https(self):
        environ = self._make_environ(scheme='https', server_port=443)
        req = Request(environ, None)
        self.assertEqual('https://example.org/trac', req.base_url)

    def test_base_url_https_host(self):
        environ = self._make_environ(scheme='https', server_port=443,
                                     HTTP_HOST='example.com')
        req = Request(environ, None)
        self.assertEqual('https://example.com/trac', req.base_url)

    def test_base_url_https_nondefaultport(self):
        environ = self._make_environ(scheme='https', server_port=8443)
        req = Request(environ, None)
        self.assertEqual('https://example.org:8443/trac', req.base_url)

    def test_base_url_proxy(self):
        environ = self._make_environ(HTTP_HOST='localhost',
                                     HTTP_X_FORWARDED_HOST='example.com')
        req = Request(environ, None)
        self.assertEqual('http://localhost/trac', req.base_url)

    def test_languages(self):
        environ = self._make_environ()
        environ['HTTP_ACCEPT_LANGUAGE'] = 'en-us,en;q=0.5'
        req = Request(environ, None)
        self.assertEqual(['en-us', 'en'], req.languages)

    def test_redirect(self):
        status_sent = []
        headers_sent = {}
        def start_response(status, headers):
            status_sent.append(status)
            headers_sent.update(dict(headers))
        environ = self._make_environ(method='HEAD')
        req = Request(environ, start_response)
        req.session = Mock(save=lambda: None)
        self.assertRaises(RequestDone, req.redirect, '/trac/test')
        self.assertEqual('302 Found', status_sent[0])
        self.assertEqual('http://example.org/trac/test',
                         headers_sent['Location'])

    def test_redirect_absolute(self):
        status_sent = []
        headers_sent = {}
        def start_response(status, headers):
            status_sent.append(status)
            headers_sent.update(dict(headers))
        environ = self._make_environ(method='HEAD')
        req = Request(environ, start_response,)
        req.session = Mock(save=lambda: None)
        self.assertRaises(RequestDone, req.redirect,
                          'http://example.com/trac/test')
        self.assertEqual('302 Found', status_sent[0])
        self.assertEqual('http://example.com/trac/test',
                         headers_sent['Location'])

    def test_redirect_with_post_and_hash_for_msie(self):
        url = 'http://example.com/trac/ticket/1#comment:2'
        msie303 = 'http://example.com/trac/ticket/1#__msie303:comment:2'

        def location(ua):
            status_sent = []
            headers_sent = {}
            def start_response(status, headers):
                status_sent.append(status)
                headers_sent.update(dict(headers))
            environ = self._make_environ(method='POST', HTTP_USER_AGENT=ua)
            req = Request(environ, start_response,)
            req.session = Mock(save=lambda: None)
            self.assertRaises(RequestDone, req.redirect, url)
            self.assertEqual('303 See Other', status_sent[0])
            return headers_sent['Location']

        # IE 11 strict mode
        self.assertEqual(url, location(
            'Mozilla/5.0 (Windows NT 6.1; Trident/7.0; rv:11.0) like Gecko'))
        # IE 11 compatibility view mode
        self.assertEqual(url, location(
            'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.1; Trident/7.0)'))
        # IE 10 strict mode
        self.assertEqual(url, location(
            'Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; Trident/6.0)'
            ))
        # IE 10 compatibility view mode
        self.assertEqual(url, location(
            'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.1; Trident/6.0)'))
        # IE 9 strict mode
        self.assertEqual(msie303, location(
            'Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)'))
        # IE 9 compatibility view mode
        self.assertEqual(msie303, location(
            'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.1; Trident/5.0)'))
        # IE 8 strict mode
        self.assertEqual(msie303, location(
            'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0)'))
        # IE 8 compatibility view mode
        self.assertEqual(msie303, location(
            'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; Trident/4.0)'))
        # IE 7
        self.assertEqual(msie303, location(
            'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1)'))
        # IE 6
        self.assertEqual(msie303, location(
            'Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1)'))

    def test_write_iterable(self):
        buf = StringIO()
        def write(data):
            buf.write(data)
        def start_response(status, headers):
            return write
        environ = self._make_environ(method='GET')

        buf = StringIO()
        req = Request(environ, start_response)
        req.send_header('Content-Type', 'text/plain;charset=utf-8')
        req.write(('Foo', 'bar', 'baz'))
        self.assertEqual('Foobarbaz', buf.getvalue())

    def test_write_unicode(self):
        buf = StringIO()
        def write(data):
            buf.write(data)
        def start_response(status, headers):
            return write
        environ = self._make_environ(method='HEAD')

        req = Request(environ, start_response)
        req.send_header('Content-Type', 'text/plain;charset=utf-8')
        req.send_header('Content-Length', 0)
        # anyway we're not supposed to send unicode, so we get a ValueError
        self.assertRaises(ValueError, req.write, u'Föö')
        self.assertRaises(ValueError, req.write, ('F', u'öo'))

    def test_send_iterable(self):
        baton = {'content': StringIO(), 'status': None, 'headers': None}
        def write(data):
            baton['content'].write(data)
        def start_response(status, headers):
            baton['status'] = status
            baton['headers'] = headers
            return write
        environ = self._make_environ(method='GET')

        def iterable():
            yield 'line1,'
            yield ''
            yield 'line2,'
            yield 'line3\n'

        req = Request(environ, start_response)
        self.assertRaises(RequestDone, req.send, iterable())
        self.assertEqual('200 Ok', baton['status'])
        self.assertEqual([('Cache-Control', 'must-revalidate'),
                          ('Expires', 'Fri, 01 Jan 1999 00:00:00 GMT'),
                          ('Content-Type', 'text/html;charset=utf-8')],
                         baton['headers'])
        self.assertEqual('line1,line2,line3\n', baton['content'].getvalue())

    def test_invalid_cookies(self):
        environ = self._make_environ(HTTP_COOKIE='bad:key=value;')
        req = Request(environ, None)
        self.assertEqual('', str(req.incookie))

    def test_multiple_cookies(self):
        environ = self._make_environ(HTTP_COOKIE='key=value1; key=value2;')
        req = Request(environ, None)
        self.assertEqual('Set-Cookie: key=value1',
                         str(req.incookie).rstrip(';'))

    def test_read(self):
        environ = self._make_environ(**{'wsgi.input': StringIO('test input')})
        req = Request(environ, None)
        self.assertEqual('test input', req.read())

    def test_read_size(self):
        environ = self._make_environ(**{'wsgi.input': StringIO('test input')})
        req = Request(environ, None)
        self.assertEqual('test', req.read(size=4))

    def _test_qs_invalid_null_bytes(self, environ):
        req = Request(environ, None)
        try:
            req.args['action']
        except HTTPBadRequest, e:
            self.assertEqual("400 Bad Request (Invalid request arguments.)",
                             unicode(e))
        else:
            self.fail("HTTPBadRequest not raised.")

    def test_qs_invalid_null_bytes_for_name(self):
        environ = self._make_environ(method='GET',
                                     **{'QUERY_STRING': 'acti\x00n=fOO'})
        self._test_qs_invalid_null_bytes(environ)

    def test_qs_invalid_null_bytes_for_value(self):
        environ = self._make_environ(method='GET',
                                     **{'QUERY_STRING': 'action=f\x00O'})
        self._test_qs_invalid_null_bytes(environ)

    def _test_invalid_null_bytes_in_form(self, form_data):
        boundary = '_BOUNDARY_'
        content_type = 'multipart/form-data; boundary="%s"' % boundary
        form_data %= {'boundary': boundary}

        environ = self._make_environ(method='POST', **{
            'wsgi.input': StringIO(form_data),
            'CONTENT_LENGTH': str(len(form_data)),
            'CONTENT_TYPE': content_type
        })
        req = Request(environ, None)

        try:
            req.args['action']
        except HTTPBadRequest, e:
            self.assertEqual("400 Bad Request (Invalid request arguments.)",
                             unicode(e))
        else:
            self.fail("HTTPBadRequest not raised.")

    def test_invalid_null_bytes_for_filename_in_form(self):
        form_data = """\
--%(boundary)s\r\n\
Content-Disposition: form-data; name="attachment"; filename="thefi\x00le.txt"\r\n\
Content-Type: text/plain\r\n\
\r\n\
The file content.\r\n\
--%(boundary)s\r\n\
Content-Disposition: form-data; name="action"\r\n\
\r\n\
new\r\n\
--%(boundary)s--\r\n\
"""
        self._test_invalid_null_bytes_in_form(form_data)

    def test_invalid_null_bytes_for_name_in_form(self):
        form_data = """\
--%(boundary)s\r\n\
Content-Disposition: form-data; name="acti\x00n"\r\n\
\r\n\
new\r\n\
--%(boundary)s--\r\n\
"""

        self._test_invalid_null_bytes_in_form(form_data)

    def test_invalid_null_bytes_for_value_in_form(self):
        form_data = """\
--%(boundary)s\r\n\
Content-Disposition: form-data; name="action"\r\n\
\r\n\
ne\x00w\r\n\
--%(boundary)s--\r\n\
"""
        self._test_invalid_null_bytes_in_form(form_data)

    def test_qs_on_post(self):
        """Make sure req.args parsing is consistent even after the backwards
        incompatible change introduced in Python 2.6.
        """
        environ = self._make_environ(method='GET',
                                     **{'QUERY_STRING': 'action=foo'})
        req = Request(environ, None)
        self.assertEqual('foo', req.args['action'])
        environ = self._make_environ(method='POST',
                                     **{'wsgi.input': StringIO('action=bar'),
                                        'CONTENT_LENGTH': '10',
                                        'CONTENT_TYPE': 'application/x-www-form-urlencoded',
                                        'QUERY_STRING': 'action=foo'})
        req = Request(environ, None)
        self.assertEqual('bar', req.args['action'])

    def test_qs_invalid_value_bytes(self):
        environ = self._make_environ(**{'QUERY_STRING': 'name=%FF'})
        req = Request(environ, None)
        self.assertRaises(HTTPBadRequest, lambda: req.arg_list)

    def test_qs_invalid_name_bytes(self):
        environ = self._make_environ(**{'QUERY_STRING': '%FF=value'})
        req = Request(environ, None)
        self.assertRaises(HTTPBadRequest, lambda: req.arg_list)


class RequestSendFileTestCase(unittest.TestCase):

    def setUp(self):
        self.status = None
        self.headers = None
        self.response = StringIO()
        self.dir = tempfile.mkdtemp(prefix='trac-')
        self.filename = os.path.join(self.dir, 'test.txt')
        self.data = 'contents\n'
        create_file(self.filename, self.data, 'wb')
        self.req = None

    def tearDown(self):
        if self.req and self.req._response:
            self.req._response.close()
        shutil.rmtree(self.dir)

    def _start_response(self, status, headers):
        self.status = status
        self.headers = dict(headers)
        def write(data):
            self.response.write(data)
        return write

    def _create_req(self, use_xsendfile=False, xsendfile_header='X-Sendfile',
                    **kwargs):
        req = Request(_make_environ(**kwargs), self._start_response)
        req.callbacks.update({'use_xsendfile': lambda r: use_xsendfile,
                              'xsendfile_header': lambda r: xsendfile_header})
        self.req = req
        return req

    def test_send_file(self):
        req = self._create_req()
        self.assertRaises(RequestDone, req.send_file, self.filename,
                          'text/plain')
        self.assertEqual('200 Ok', self.status)
        self.assertEqual('text/plain', self.headers['Content-Type'])
        self.assertEqual(str(len(self.data)), self.headers['Content-Length'])
        self.assertNotIn('X-Sendfile', self.headers)
        self.assertEqual(self.data, ''.join(req._response))
        self.assertEqual('', self.response.getvalue())

    def test_send_file_with_xsendfile(self):
        req = self._create_req(use_xsendfile=True)
        self.assertRaises(RequestDone, req.send_file, self.filename,
                          'text/plain')
        self.assertEqual('200 Ok', self.status)
        self.assertEqual('text/plain', self.headers['Content-Type'])
        self.assertEqual(self.filename, self.headers['X-Sendfile'])
        self.assertEqual(None, req._response)
        self.assertEqual('', self.response.getvalue())

    def test_send_file_with_xsendfile_header(self):
        req = self._create_req(use_xsendfile=True,
                               xsendfile_header='X-Accel-Redirect')
        self.assertRaises(RequestDone, req.send_file, self.filename,
                          'text/plain')
        self.assertEqual('200 Ok', self.status)
        self.assertEqual('text/plain', self.headers['Content-Type'])
        self.assertEqual(self.filename, self.headers['X-Accel-Redirect'])
        self.assertNotIn('X-Sendfile', self.headers)
        self.assertEqual(None, req._response)
        self.assertEqual('', self.response.getvalue())

    def test_send_file_with_xsendfile_and_empty_header(self):
        req = self._create_req(use_xsendfile=True, xsendfile_header='')
        self.assertRaises(RequestDone, req.send_file, self.filename,
                          'text/plain')
        self.assertEqual('200 Ok', self.status)
        self.assertEqual('text/plain', self.headers['Content-Type'])
        self.assertEqual(str(len(self.data)), self.headers['Content-Length'])
        self.assertNotIn('X-Sendfile', self.headers)
        self.assertEqual(self.data, ''.join(req._response))
        self.assertEqual('', self.response.getvalue())


class SendErrorTestCase(unittest.TestCase):

    def setUp(self):
        self.env = EnvironmentStub()

    def tearDown(self):
        self.env.reset_db()

    def test_trac_error(self):
        content = self._send_error(error_klass=TracError)
        self.assertIn('<p class="message">Oops!</p>', content)
        self.assertNotIn('<strong>Trac detected an internal error:</strong>',
                         content)
        self.assertNotIn('There was an internal error in Trac.', content)

    def test_internal_error_for_non_admin(self):
        content = self._send_error(perm={})
        self.assertIn('There was an internal error in Trac.', content)
        self.assertIn('<p>To that end, you could', content)
        self.assertNotIn('This is probably a local installation issue.',
                         content)
        self.assertNotIn('<h2>Found a bug in Trac?</h2>', content)

    def test_internal_error_with_admin_trac_for_non_admin(self):
        content = self._send_error(perm={},
                                   admin_trac_url='http://example.org/admin')
        self.assertIn('There was an internal error in Trac.', content)
        self.assertIn('<p>To that end, you could', content)
        self.assertIn(' action="http://example.org/admin/newticket#"', content)
        self.assertNotIn('This is probably a local installation issue.',
                         content)
        self.assertNotIn('<h2>Found a bug in Trac?</h2>', content)

    def test_internal_error_without_admin_trac_for_non_admin(self):
        content = self._send_error(perm={}, admin_trac_url='')
        self.assertIn('There was an internal error in Trac.', content)
        self.assertNotIn('<p>To that end, you could', content)
        self.assertNotIn('This is probably a local installation issue.',
                         content)
        self.assertNotIn('<h2>Found a bug in Trac?</h2>', content)

    def test_internal_error_for_admin(self):
        content = self._send_error()
        self.assertNotIn('There was an internal error in Trac.', content)
        self.assertIn('This is probably a local installation issue.', content)
        self.assertNotIn('a ticket at the admin Trac to report', content)
        self.assertIn('<h2>Found a bug in Trac?</h2>', content)
        self.assertIn('<p>Otherwise, please', content)
        self.assertIn(' action="http://example.org/tracker/newticket"',
                      content)

    def test_internal_error_with_admin_trac_for_admin(self):
        content = self._send_error(admin_trac_url='http://example.org/admin')
        self.assertNotIn('There was an internal error in Trac.', content)
        self.assertIn('This is probably a local installation issue.', content)
        self.assertIn('a ticket at the admin Trac to report', content)
        self.assertIn(' action="http://example.org/admin/newticket#"', content)
        self.assertIn('<h2>Found a bug in Trac?</h2>', content)
        self.assertIn('<p>Otherwise, please', content)
        self.assertIn(' action="http://example.org/tracker/newticket"',
                      content)

    def test_internal_error_without_admin_trac_for_admin(self):
        content = self._send_error(admin_trac_url='')
        self.assertNotIn('There was an internal error in Trac.', content)
        self.assertIn('This is probably a local installation issue.', content)
        self.assertNotIn('a ticket at the admin Trac to report', content)
        self.assertIn('<h2>Found a bug in Trac?</h2>', content)
        self.assertIn('<p>Otherwise, please', content)
        self.assertIn(' action="http://example.org/tracker/newticket"',
                      content)

    def _send_error(self, admin_trac_url='.', perm=None,
                    error_klass=ValueError):
        self.env.config.set('project', 'admin_trac_url', admin_trac_url)
        self.assertEquals(admin_trac_url, self.env.project_admin_trac_url)

        content = StringIO()
        result = {'status': None, 'headers': []}
        def write(data):
            content.write(data)
        def start_response(status, headers, exc_info=None):
            result['status'] = status
            result['headers'].extend(headers)
            return write
        environ = _make_environ()
        req = _make_req(environ, start_response)
        try:
            raise error_klass('Oops!')
        except:
            exc_info = sys.exc_info()
        data = {'title': 'Internal Error',
                'type': ('internal', 'TracError')[error_klass is TracError],
                'message': 'Oops!', 'traceback': None, 'frames': [],
                'shorten_line': shorten_line,
                'plugins': [], 'faulty_plugins': [],
                'tracker': 'http://example.org/tracker', 'tracker_args': {},
                'description': '', 'description_en': '',
                'get_systeminfo': lambda: ()}
        if perm is not None:
            data['perm'] = perm

        self.assertRaises(RequestDone, req.send_error, exc_info, env=self.env,
                          data=data)
        content = content.getvalue().decode('utf-8')
        self.assertIn('<!DOCTYPE ', content)
        self.assertEquals('500', result['status'].split()[0])
        self.assertIn(('Content-Type', 'text/html;charset=utf-8'),
                      result['headers'])
        return content


class ParseArgListTestCase(unittest.TestCase):

    def test_qs_str(self):
        args = parse_arg_list('k%C3%A9y=resum%C3%A9&r%C3%A9sum%C3%A9')
        self.assertTrue(unicode, type(args[0][0]))
        self.assertTrue(unicode, type(args[0][1]))
        self.assertEqual(u'kéy', args[0][0])
        self.assertEqual(u'resumé', args[0][1])
        self.assertTrue(unicode, type(args[1][0]))
        self.assertEqual(u'résumé', args[1][0])

    def test_qs_str_with_prefix(self):
        """The leading `?` should be stripped from the query string."""
        args = parse_arg_list('?k%C3%A9y=resum%C3%A9&r%C3%A9sum%C3%A9')
        self.assertTrue(unicode, type(args[0][0]))
        self.assertTrue(unicode, type(args[0][1]))
        self.assertEqual(u'kéy', args[0][0])
        self.assertEqual(u'resumé', args[0][1])
        self.assertTrue(unicode, type(args[1][0]))
        self.assertEqual(u'résumé', args[1][0])

    def test_qs_unicode(self):
        args = parse_arg_list(u'ké%3Dy=re%26su=mé&résu%26mé')
        self.assertTrue(unicode, type(args[0][0]))
        self.assertTrue(unicode, type(args[0][1]))
        self.assertEqual(u'ké=y', args[0][0])
        self.assertEqual(u're&su=mé', args[0][1])
        self.assertTrue(unicode, type(args[1][0]))
        self.assertEqual(u'résu&mé', args[1][0])


class HTTPExceptionTestCase(unittest.TestCase):

    def test_tracerror_with_string_as_argument(self):
        e1 = TracError('the message')
        e2 = HTTPInternalError(e1)
        self.assertEqual('500 Trac Error (the message)', unicode(e2))

    def test_tracerror_with_fragment_as_argument(self):
        e1 = TracError(tag(tag.b('the message')))
        e2 = HTTPInternalError(e1)
        self.assertEqual('500 Trac Error (<b>the message</b>)', unicode(e2))

    def test_exception_with_string_as_argument(self):
        e1 = Exception('the message')
        e2 = HTTPInternalError(e1)
        self.assertEqual('500 Internal Server Error (the message)',
                         unicode(e2))

    def test_exception_with_fragment_as_argument(self):
        e1 = Exception(tag(tag.b('the message')))
        e2 = HTTPInternalError(e1)
        self.assertEqual('500 Internal Server Error (<b>the message</b>)',
                         unicode(e2))


def suite():
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(RequestTestCase))
    suite.addTest(unittest.makeSuite(RequestSendFileTestCase))
    suite.addTest(unittest.makeSuite(SendErrorTestCase))
    suite.addTest(unittest.makeSuite(ParseArgListTestCase))
    suite.addTest(unittest.makeSuite(HTTPExceptionTestCase))
    return suite


if __name__ == '__main__':
    unittest.main(defaultTest='suite')
