# -*- coding: utf-8 -*-
#
# Copyright (C) 2004-2013 Edgewall Software
# All rights reserved.
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at http://trac.edgewall.org/wiki/TracLicense.
#
# This software consists of voluntary contributions made by many
# individuals. For the exact contribution history, see the revision
# history and logs, available at http://trac.edgewall.org/log/.

from datetime import datetime, timedelta
import difflib
import re
import unittest

from trac.mimeview.api import Mimeview
from trac.test import Mock, EnvironmentStub, MockPerm, locale_en
from trac.ticket.api import TicketSystem
from trac.ticket.model import Milestone, Severity, Ticket, Version
from trac.ticket.query import Query, QueryModule, TicketQueryMacro
from trac.util.datefmt import utc
from trac.web.api import arg_list_to_args, parse_arg_list
from trac.web.chrome import web_context
from trac.web.href import Href
from trac.wiki.formatter import LinkFormatter
from trac.wiki.tests import formatter

# Note: we don't want to replicate 1:1 all the SQL dialect abstraction
#       methods from the trac.db layer here.

class QueryTestCase(unittest.TestCase):

    n_tickets = 10

    def prettifySQL(self, sql):
        """Returns a prettified version of the SQL as a list of lines to help
        in creating a useful diff between two SQL statements."""
        pretty = []
        for line in sql.split('\n'):
            pretty.extend([ "%s,\n" % x for x in line.split(',')])
        return pretty

    def assertEqualSQL(self, sql, correct_sql):
        sql_split = self.prettifySQL(sql)
        correct_sql_split = self.prettifySQL(correct_sql)
        sql_diff = ''.join(list(
            difflib.unified_diff(correct_sql_split, sql_split)
        ))
        failure_message = "%r != %r\n" % (sql, correct_sql) + sql_diff
        self.assertEqual(sql, correct_sql, failure_message)

    def setUp(self):
        self.env = EnvironmentStub(default_data=True)
        self.req = Mock(href=self.env.href, authname='anonymous', tz=utc,
                        locale=locale_en, lc_time=locale_en)
        self.tktids = self._insert_tickets(
            owner=[None, '', 'someone', 'someone_else', 'none'],
            status=[None, '', 'new', 'assigned', 'reopened', 'closed'],
            priority=[None, '', 'blocker', 'critical', 'major', 'minor',
                      'trivial'],
            milestone=[None, '', 'milestone1', 'milestone2'],
            version=[None, '', '0.0', 'version1', '1.0', '2.0'],
            keywords=[None, '', 'foo', 'bar', 'baz', 'foo bar', 'bar baz',
                      'foo baz', 'foo bar baz'])
        dt = datetime(2008, 7, 1, 12, tzinfo=utc)
        with self.env.db_transaction:
            for name in ('milestone1', 'milestone2'):
                milestone = Milestone(self.env, name)
                milestone.due = dt
                milestone.update()
            for name in ('1.0', '2.0'):
                version = Version(self.env, name)
                version.time = dt
                version.update()
            for name in ('urgent', 'high', 'medium'):
                severity = Severity(self.env)
                severity.name = name
                severity.insert()
        tktsys = TicketSystem(self.env)
        tktsys.reset_ticket_fields()
        del tktsys.custom_fields

    def tearDown(self):
        self.env.reset_db()

    def _insert_tickets(self, owner, status, priority, milestone, version,
                        keywords):
        when = datetime(2008, 7, 1, 12, 34, 56, 987654, utc)
        with self.env.db_transaction:
            ids = []
            for idx in xrange(self.n_tickets):
                t = Ticket(self.env)
                t['summary'] = 'Summary %d' % idx
                t['owner'] = owner[idx % len(owner)]
                t['status'] = status[idx % len(status)]
                t['priority'] = priority[idx % len(priority)]
                t['milestone'] = milestone[idx % len(milestone)]
                t['version'] = version[idx % len(version)]
                t['keywords'] = keywords[idx % len(keywords)]
                ids.append(t.insert(when=when + timedelta(days=idx * 10)))
                t.save_changes(comment='...',
                               when=when + timedelta(days=idx * 10 + 1))
        return ids

    def _update_tickets(self, name, values):
        with self.env.db_transaction:
            for idx, tktid in enumerate(self.tktids):
                t = Ticket(self.env, tktid)
                t[name] = values[idx % len(values)]
                t.save_changes()

    def test_all_ordered_by_id(self):
        query = Query(self.env, order='id')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual([], args)
        tickets = query.execute(self.req)
        self.assertEqual(self.n_tickets, len(tickets))
        self.assertTrue(tickets[0]['id'] < tickets[-1]['id'])

    def test_all_ordered_by_id_desc(self):
        query = Query(self.env, order='id', desc=1)
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
ORDER BY COALESCE(t.id,0)=0 DESC,t.id DESC""")
        self.assertEqual([], args)
        tickets = query.execute(self.req)
        self.assertEqual(self.n_tickets, len(tickets))
        self.assertTrue(tickets[0]['id'] > tickets[-1]['id'])

    def test_all_ordered_by_id_verbose(self):
        query = Query(self.env, order='id', verbose=1)
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.reporter AS reporter,t.description AS description,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual([], args)
        tickets = query.execute(self.req)
        self.assertEqual(self.n_tickets, len(tickets))

    def test_all_ordered_by_id_from_unicode(self):
        query = Query.from_string(self.env, u'order=id')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual([], args)
        tickets = query.execute(self.req)
        self.assertEqual(self.n_tickets, len(tickets))

    def test_all_ordered_by_priority(self):
        query = Query(self.env) # priority is default order
        sql, args = query.get_sql()
        with self.env.db_query as db:
            cast_priority = db.cast('priority.value', 'int')
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
ORDER BY COALESCE(priority.value,'')='',%(cast_priority)s,t.id""" % {
          'cast_priority': cast_priority})
        self.assertEqual([], args)
        tickets = query.execute(self.req)
        self.assertEqual(['blocker', 'blocker', 'critical', 'major', 'minor',
                          'trivial', '', '', '', ''],
                         [t['priority'] for t in tickets])

    def test_all_ordered_by_priority_desc(self):
        query = Query(self.env, desc=1) # priority is default order
        sql, args = query.get_sql()
        with self.env.db_query as db:
            cast_priority = db.cast('priority.value', 'int')
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
ORDER BY COALESCE(priority.value,'')='' DESC,%(cast_priority)s DESC,t.id""" % {
          'cast_priority': cast_priority})
        self.assertEqual([], args)
        tickets = query.execute(self.req)
        self.assertEqual(['', '', '', '', 'trivial', 'minor', 'major',
                          'critical', 'blocker', 'blocker'],
                         [t['priority'] for t in tickets])

    def test_all_ordered_by_version(self):
        query = Query(self.env, order='version')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.version AS version,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
  LEFT OUTER JOIN version ON (version.name=version)
ORDER BY COALESCE(t.version,'')='',COALESCE(version.time,0)=0,version.time,t.version,t.id""")
        self.assertEqual([], args)
        tickets = query.execute(self.req)
        self.assertEqual(['1.0', '2.0', '0.0', '0.0', 'version1', 'version1',
                          '', '', '', ''],
                         [t['version'] for t in tickets])

    def test_all_ordered_by_version_desc(self):
        query = Query(self.env, order='version', desc=1)
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.version AS version,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
  LEFT OUTER JOIN version ON (version.name=version)
ORDER BY COALESCE(t.version,'')='' DESC,COALESCE(version.time,0)=0 DESC,version.time DESC,t.version DESC,t.id""")
        self.assertEqual([], args)
        tickets = query.execute(self.req)
        self.assertEqual(['', '', '', '', 'version1', 'version1', '0.0', '0.0',
                          '2.0', '1.0'],
                         [t['version'] for t in tickets])

    def test_constrained_by_milestone(self):
        query = Query.from_string(self.env, 'milestone=milestone1', order='id')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.component AS component,t.time AS time,t.changetime AS changetime,t.milestone AS milestone,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE ((COALESCE(t.milestone,'')=%s))
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual(['milestone1'], args)
        tickets = query.execute(self.req)
        self.assertEqual(['milestone1', 'milestone1'],
                         [t['milestone'] for t in tickets])

    def test_all_grouped_by_milestone(self):
        query = Query(self.env, order='id', group='milestone')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.component AS component,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
  LEFT OUTER JOIN milestone ON (milestone.name=milestone)
ORDER BY COALESCE(t.milestone,'')='',COALESCE(milestone.completed,0)=0,milestone.completed,COALESCE(milestone.due,0)=0,milestone.due,t.milestone,COALESCE(t.id,0)=0,t.id""")
        self.assertEqual([], args)
        tickets = query.execute(self.req)
        self.assertEqual(['milestone1', 'milestone1', 'milestone2',
                          'milestone2', '', '', '', '', '', ''],
                         [t['milestone'] for t in tickets])

    def test_all_grouped_by_milestone_desc(self):
        query = Query(self.env, order='id', group='milestone', groupdesc=1)
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.component AS component,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
  LEFT OUTER JOIN milestone ON (milestone.name=milestone)
ORDER BY COALESCE(t.milestone,'')='' DESC,COALESCE(milestone.completed,0)=0 DESC,milestone.completed DESC,COALESCE(milestone.due,0)=0 DESC,milestone.due DESC,t.milestone DESC,COALESCE(t.id,0)=0,t.id""")
        self.assertEqual([], args)
        tickets = query.execute(self.req)
        self.assertEqual(['', '', '', '', '', '', 'milestone2', 'milestone2',
                          'milestone1', 'milestone1'],
                         [t['milestone'] for t in tickets])

    def test_grouped_by_priority(self):
        query = Query(self.env, group='priority')
        sql, args = query.get_sql()
        with self.env.db_query as db:
            cast_priority = db.cast('priority.value', 'int')
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.milestone AS milestone,t.component AS component,t.priority AS priority,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
ORDER BY COALESCE(priority.value,'')='',%(cast_priority)s,t.id""" % {
          'cast_priority': cast_priority})
        self.assertEqual([], args)
        tickets = query.execute(self.req)
        self.assertEqual(['blocker', 'blocker', 'critical', 'major', 'minor',
                          'trivial', '', '', '', ''],
                         [t['priority'] for t in tickets])

    def test_constrained_by_milestone_not(self):
        query = Query.from_string(self.env, 'milestone!=milestone1', order='id')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.milestone AS milestone,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE ((COALESCE(t.milestone,'')!=%s))
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual(['milestone1'], args)
        tickets = query.execute(self.req)
        self.assertEqual(['', '', 'milestone2', '', '', 'milestone2', '', ''],
                         [t['milestone'] for t in tickets])

    def test_constrained_by_status(self):
        query = Query.from_string(self.env, 'status=new|assigned|reopened',
                                  order='id')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.status AS status,t.owner AS owner,t.type AS type,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE (COALESCE(t.status,'') IN (%s,%s,%s))
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual(['new', 'assigned', 'reopened'], args)
        tickets = query.execute(self.req)
        self.assertEqual(['new', 'assigned', 'reopened', 'new', 'assigned'],
                         [t['status'] for t in tickets])

    def test_constrained_by_owner_containing(self):
        query = Query.from_string(self.env, 'owner~=someone', order='id')
        sql, args = query.get_sql()
        with self.env.db_query as db:
            like = db.like()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE ((COALESCE(t.owner,'') %(like)s))
ORDER BY COALESCE(t.id,0)=0,t.id""" % {'like': like})
        self.assertEqual(['%someone%'], args)
        tickets = query.execute(self.req)
        self.assertEqual(['someone', 'someone_else', 'someone',
                          'someone_else'],
                         [t['owner'] for t in tickets])

    def test_constrained_by_owner_not_containing(self):
        query = Query.from_string(self.env, 'owner!~=someone', order='id')
        sql, args = query.get_sql()
        with self.env.db_query as db:
            like = db.like()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE ((COALESCE(t.owner,'') NOT %(like)s))
ORDER BY COALESCE(t.id,0)=0,t.id""" % {'like': like})
        self.assertEqual(['%someone%'], args)
        tickets = query.execute(self.req)
        self.assertEqual(['', '', 'none', '', '', 'none'],
                         [t['owner'] for t in tickets])

    def test_constrained_by_owner_beginswith(self):
        query = Query.from_string(self.env, 'owner^=someone', order='id')
        sql, args = query.get_sql()
        with self.env.db_query as db:
            like = db.like()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE ((COALESCE(t.owner,'') %(like)s))
ORDER BY COALESCE(t.id,0)=0,t.id""" % {'like': like})
        self.assertEqual(['someone%'], args)
        tickets = query.execute(self.req)
        self.assertEqual(['someone', 'someone_else', 'someone',
                          'someone_else'],
                         [t['owner'] for t in tickets])

    def test_constrained_by_owner_endswith(self):
        query = Query.from_string(self.env, 'owner$=someone', order='id')
        sql, args = query.get_sql()
        with self.env.db_query as db:
            like = db.like()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE ((COALESCE(t.owner,'') %(like)s))
ORDER BY COALESCE(t.id,0)=0,t.id""" % {'like': like})
        self.assertEqual(['%someone'], args)
        tickets = query.execute(self.req)
        self.assertEqual(['someone', 'someone'], [t['owner'] for t in tickets])

    def test_constrained_by_custom_field(self):
        self.env.config.set('ticket-custom', 'foo', 'text')
        self._update_tickets('foo', [None, '', 'something'])
        query = Query.from_string(self.env, 'foo=something', order='id')
        sql, args = query.get_sql()
        with self.env.db_query as db:
            foo = db.quote('foo')
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value,%(foo)s.value AS %(foo)s
FROM ticket AS t
  LEFT OUTER JOIN ticket_custom AS %(foo)s ON (%(foo)s.ticket=t.id AND %(foo)s.name='foo')
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE ((COALESCE(%(foo)s.value,'')=%%s))
ORDER BY COALESCE(t.id,0)=0,t.id"""
        % {'foo': foo})
        self.assertEqual(['something'], args)
        tickets = query.execute(self.req)
        self.assertEqual(['something'] * 3, [t['foo'] for t in tickets])

    def test_grouped_by_custom_field(self):
        self.env.config.set('ticket-custom', 'foo', 'text')
        self._update_tickets('foo', [None, '', 'something'])
        query = Query(self.env, group='foo', order='id')
        sql, args = query.get_sql()
        with self.env.db_query as db:
            foo = db.quote('foo')
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value,%(foo)s.value AS %(foo)s
FROM ticket AS t
  LEFT OUTER JOIN ticket_custom AS %(foo)s ON (%(foo)s.ticket=t.id AND %(foo)s.name='foo')
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
ORDER BY COALESCE(%(foo)s.value,'')='',%(foo)s.value,COALESCE(t.id,0)=0,t.id"""
        % {'foo': foo})
        self.assertEqual([], args)
        tickets = query.execute(self.req)
        self.assertEqual(['something'] * 3 + [''] * 7,
                         [t['foo'] for t in tickets])

    def test_constrained_by_id_ranges(self):
        query = Query.from_string(self.env, 'id=42,44,51-55&order=id')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE ((t.id BETWEEN %s AND %s OR t.id IN (42,44)))
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual([51, 55], args)

    def test_constrained_by_id_and_custom_field(self):
        self.env.config.set('ticket-custom', 'foo', 'text')
        ticket = Ticket(self.env)
        ticket['reporter'] = 'joe'
        ticket['summary'] = 'Foo'
        ticket['foo'] = 'blah'
        ticket.insert()

        query = Query.from_string(self.env, 'id=%d-42&foo=blah' % ticket.id)
        tickets = query.execute(self.req)
        self.assertEqual(1, len(tickets))
        self.assertEqual(ticket.id, tickets[0]['id'])

        query = Query.from_string(self.env, 'id=%d,42&foo=blah' % ticket.id)
        tickets = query.execute(self.req)
        self.assertEqual(1, len(tickets))
        self.assertEqual(ticket.id, tickets[0]['id'])

        query = Query.from_string(self.env, 'id=%d,42,43-84&foo=blah' %
                                            ticket.id)
        tickets = query.execute(self.req)
        self.assertEqual(1, len(tickets))
        self.assertEqual(ticket.id, tickets[0]['id'])

    def _get_join_tables(self, sql):
        return sorted(match.group(1)
                      for match in re.finditer(r'\bLEFT OUTER JOIN (\w+)',
                                               sql))

    def test_query_using_joins(self):
        fields = ['col_%02d' % i for i in xrange(100)]
        for f in fields:
            self.env.config.set('ticket-custom', f, 'text')
        with self.env.db_transaction as db:
            quoted_cols = dict((f, db.quote(f)) for f in fields)
            ticket = Ticket(self.env)
            ticket['reporter'] = 'joe'
            ticket['summary'] = 'Foo'
            for idx, f in enumerate(fields):
                ticket[f] = '%d.%s' % (idx, f)
            ticket.insert()

        query = Query.from_string(
            self.env, 'col_12=12.col_12&' +
                      'order=resolution&group=severity&col=id&col=summary' +
                      ''.join('&col=col_%02d' % idx for idx in xrange(28)))
        sql, args = query.get_sql()
        self.assertEqual(['enum'] * 3 + ['ticket_custom'] * 28,
                         self._get_join_tables(sql))
        tickets = query.execute(self.req)
        self.assertEqual(1, len(tickets))

        query = Query.from_string(
            self.env, 'col_12=12.col_12&' +
                      'order=milestone&group=version&col=id&col=summary' +
                      ''.join('&col=col_%02d' % idx for idx in xrange(28)))
        sql, args = query.get_sql()
        self.assertEqual(['enum', 'milestone'] + ['ticket_custom'] * 28 +
                         ['version'],
                         self._get_join_tables(sql))
        tickets = query.execute(self.req)
        self.assertEqual(1, len(tickets))

        query = Query.from_string(
            self.env, 'col_12=12.col_12&' +
                      'order=resolution&group=severity&col=id&col=summary' +
                      ''.join('&col=col_%02d' % idx for idx in xrange(29)))
        sql, args = query.get_sql()
        self.assertEqual(['enum'] * 3, self._get_join_tables(sql))
        tickets = query.execute(self.req)
        self.assertEqual(ticket.id, tickets[0]['id'])
        self.assertEqual(1, len(tickets))

        query = Query.from_string(
            self.env, 'col_12=12.col_12&' +
                      'order=milestone&group=version&col=id&col=summary' +
                      ''.join('&col=col_%02d' % idx for idx in xrange(29)))
        sql, args = query.get_sql()
        self.assertEqual(['enum', 'milestone', 'version'],
                         self._get_join_tables(sql))
        tickets = query.execute(self.req)
        self.assertEqual(1, len(tickets))

    def test_too_many_custom_fields(self):
        fields = ['col_%02d' % i for i in xrange(100)]
        for f in fields:
            self.env.config.set('ticket-custom', f, 'text')

        ticket = Ticket(self.env)
        ticket['reporter'] = 'joe'
        ticket['summary'] = 'Foo'
        for idx, f in enumerate(fields):
            ticket[f] = '%d.%s' % (idx, f)
        ticket.insert()

        string = 'col_00=0.col_00&order=id&col=id&col=reporter&col=summary' + \
                 ''.join('&col=' + f for f in fields)
        query = Query.from_string(self.env, string)
        tickets = query.execute(self.req)
        self.assertEqual(ticket.id, tickets[0]['id'])
        self.assertEqual('joe', tickets[0]['reporter'])
        self.assertEqual('Foo', tickets[0]['summary'])
        self.assertEqual('0.col_00', tickets[0]['col_00'])
        self.assertEqual('99.col_99', tickets[0]['col_99'])

        query = Query.from_string(self.env, 'col_00=notfound')
        self.assertEqual([], query.execute(self.req))

    def test_constrained_by_multiple_owners(self):
        query = Query.from_string(self.env, 'owner=someone|someone_else',
                                  order='id')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE (COALESCE(t.owner,'') IN (%s,%s))
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual(['someone', 'someone_else'], args)
        tickets = query.execute(self.req)
        self.assertEqual(['someone', 'someone_else', 'someone',
                          'someone_else'],
                         [t['owner'] for t in tickets])

    def test_constrained_by_multiple_owners_not(self):
        query = Query.from_string(self.env, 'owner!=someone|someone_else',
                                  order='id')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE (COALESCE(t.owner,'') NOT IN (%s,%s))
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual(['someone', 'someone_else'], args)
        tickets = query.execute(self.req)
        self.assertEqual(['', '', 'none', '', '', 'none'],
                         [t['owner'] for t in tickets])

    def test_constrained_by_multiple_owners_contain(self):
        query = Query.from_string(self.env, 'owner~=someone|someone_else',
                                  order='id')
        sql, args = query.get_sql()
        with self.env.db_query as db:
            like = db.like()
        self.assertEqual(['%someone%', '%someone/_else%'], args)
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE ((COALESCE(t.owner,'') %(like)s OR COALESCE(t.owner,'') %(like)s))
ORDER BY COALESCE(t.id,0)=0,t.id""" % {'like': like})
        tickets = query.execute(self.req)
        self.assertEqual(['someone', 'someone_else', 'someone',
                          'someone_else'],
                         [t['owner'] for t in tickets])

    def test_constrained_by_an_empty_value(self):
        query = Query.from_string(self.env, 'owner=', order='id')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE ((COALESCE(t.owner,'')=%s))
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual([''], args)
        tickets = query.execute(self.req)
        self.assertEqual(['', '', '', ''], [t['owner'] for t in tickets])

    def test_constrained_by_an_empty_value_not(self):
        query = Query.from_string(self.env, 'owner!=', order='id')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE ((COALESCE(t.owner,'')!=%s))
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual([''], args)
        tickets = query.execute(self.req)
        self.assertEqual(['someone', 'someone_else', 'none', 'someone',
                          'someone_else', 'none'],
                         [t['owner'] for t in tickets])

    def test_constrained_by_empty_values(self):
        query = Query.from_string(self.env, 'owner=|', order='id')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE (COALESCE(t.owner,'') IN (%s,%s))
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual(['', ''], args)
        tickets = query.execute(self.req)
        self.assertEqual(['', '', '', ''], [t['owner'] for t in tickets])

    def test_constrained_by_empty_values_not(self):
        query = Query.from_string(self.env, 'owner!=|', order='id')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE (COALESCE(t.owner,'') NOT IN (%s,%s))
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual(['', ''], args)
        tickets = query.execute(self.req)
        self.assertEqual(['someone', 'someone_else', 'none', 'someone',
                          'someone_else', 'none'],
                         [t['owner'] for t in tickets])

    def test_constrained_by_empty_value_contains(self):
        query = Query.from_string(self.env, 'owner~=|', order='id')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual([], args)
        tickets = query.execute(self.req)
        self.assertEqual(['', '', 'someone', 'someone_else', 'none', '', '',
                          'someone', 'someone_else', 'none'],
                         [t['owner'] for t in tickets])

    def test_constrained_by_empty_value_startswith(self):
        query = Query.from_string(self.env, 'owner^=|', order='id')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual([], args)
        tickets = query.execute(self.req)
        self.assertEqual(['', '', 'someone', 'someone_else', 'none', '', '',
                          'someone', 'someone_else', 'none'],
                         [t['owner'] for t in tickets])

    def test_constrained_by_empty_value_endswith(self):
        query = Query.from_string(self.env, 'owner$=|', order='id')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual([], args)
        tickets = query.execute(self.req)
        self.assertEqual(['', '', 'someone', 'someone_else', 'none', '', '',
                          'someone', 'someone_else', 'none'],
                         [t['owner'] for t in tickets])

    def test_constrained_by_time_range(self):
        query = Query.from_string(self.env, 'created=2008-08-01..2008-09-01', order='id')
        sql, args = query.get_sql(self.req)
        with self.env.db_query as db:
            cast_time = db.cast('t.time', 'int64')
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.time AS time,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE (((%(cast_time)s>=%%s AND %(cast_time)s<%%s)))
ORDER BY COALESCE(t.id,0)=0,t.id""" % {
          'cast_time': cast_time})
        self.assertEqual([1217548800000000L, 1220227200000000L], args)
        tickets = query.execute(self.req)
        self.assertEqual(['2008-08-10T12:34:56.987654+00:00',
                          '2008-08-20T12:34:56.987654+00:00',
                          '2008-08-30T12:34:56.987654+00:00'],
                         [t['time'].isoformat() for t in tickets])

    def test_constrained_by_time_range_exclusion(self):
        query = Query.from_string(self.env, 'created!=2008-08-01..2008-09-01', order='id')
        sql, args = query.get_sql(self.req)
        with self.env.db_query as db:
            cast_time = db.cast('t.time', 'int64')
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.time AS time,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE ((NOT (%(cast_time)s>=%%s AND %(cast_time)s<%%s)))
ORDER BY COALESCE(t.id,0)=0,t.id""" % {
          'cast_time': cast_time})
        self.assertEqual([1217548800000000L, 1220227200000000L], args)
        tickets = query.execute(self.req)
        self.assertEqual(['2008-07-01T12:34:56.987654+00:00',
                          '2008-07-11T12:34:56.987654+00:00',
                          '2008-07-21T12:34:56.987654+00:00',
                          '2008-07-31T12:34:56.987654+00:00',
                          '2008-09-09T12:34:56.987654+00:00',
                          '2008-09-19T12:34:56.987654+00:00',
                          '2008-09-29T12:34:56.987654+00:00'],
                         [t['time'].isoformat() for t in tickets])

    def test_constrained_by_time_range_open_right(self):
        query = Query.from_string(self.env, 'created=2008-08-01..', order='id')
        sql, args = query.get_sql(self.req)
        with self.env.db_query as db:
            cast_time = db.cast('t.time', 'int64')
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.time AS time,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE ((%(cast_time)s>=%%s))
ORDER BY COALESCE(t.id,0)=0,t.id""" % {
          'cast_time': cast_time})
        self.assertEqual([1217548800000000L], args)
        tickets = query.execute(self.req)
        self.assertEqual(['2008-08-10T12:34:56.987654+00:00',
                          '2008-08-20T12:34:56.987654+00:00',
                          '2008-08-30T12:34:56.987654+00:00',
                          '2008-09-09T12:34:56.987654+00:00',
                          '2008-09-19T12:34:56.987654+00:00',
                          '2008-09-29T12:34:56.987654+00:00'],
                         [t['time'].isoformat() for t in tickets])

    def test_constrained_by_time_range_open_left(self):
        query = Query.from_string(self.env, 'created=..2008-09-01', order='id')
        sql, args = query.get_sql(self.req)
        with self.env.db_query as db:
            cast_time = db.cast('t.time', 'int64')
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.time AS time,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE ((%(cast_time)s<%%s))
ORDER BY COALESCE(t.id,0)=0,t.id""" % {
          'cast_time': cast_time})
        self.assertEqual([1220227200000000L], args)
        tickets = query.execute(self.req)
        self.assertEqual(['2008-07-01T12:34:56.987654+00:00',
                          '2008-07-11T12:34:56.987654+00:00',
                          '2008-07-21T12:34:56.987654+00:00',
                          '2008-07-31T12:34:56.987654+00:00',
                          '2008-08-10T12:34:56.987654+00:00',
                          '2008-08-20T12:34:56.987654+00:00',
                          '2008-08-30T12:34:56.987654+00:00'],
                         [t['time'].isoformat() for t in tickets])

    def test_constrained_by_time_range_modified(self):
        query = Query.from_string(self.env, 'modified=2008-08-01..2008-09-01', order='id')
        sql, args = query.get_sql(self.req)
        with self.env.db_query as db:
            cast_changetime = db.cast('t.changetime', 'int64')
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.changetime AS changetime,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.time AS time,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE (((%(cast_changetime)s>=%%s AND %(cast_changetime)s<%%s)))
ORDER BY COALESCE(t.id,0)=0,t.id""" % {
          'cast_changetime': cast_changetime})
        self.assertEqual([1217548800000000L, 1220227200000000L], args)
        tickets = query.execute(self.req)
        self.assertEqual(['2008-08-01T12:34:56.987654+00:00',
                          '2008-08-11T12:34:56.987654+00:00',
                          '2008-08-21T12:34:56.987654+00:00',
                          '2008-08-31T12:34:56.987654+00:00'],
                         [t['changetime'].isoformat() for t in tickets])

    def test_constrained_by_keywords(self):
        query = Query.from_string(self.env, 'keywords~=foo -bar baz',
                                  order='id')
        sql, args = query.get_sql()
        with self.env.db_query as db:
            like = db.like()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.keywords AS keywords,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE (((COALESCE(t.keywords,'') %(like)s AND COALESCE(t.keywords,'') NOT %(like)s AND COALESCE(t.keywords,'') %(like)s)))
ORDER BY COALESCE(t.id,0)=0,t.id""" % {'like': like})
        self.assertEqual(['%foo%', '%bar%', '%baz%'], args)
        tickets = query.execute(self.req)
        self.assertEqual(['foo baz'], [t['keywords'] for t in tickets])

    def test_constrained_by_keywords_not(self):
        query = Query.from_string(self.env, 'keywords!~=foo -bar baz',
                                  order='id')
        sql, args = query.get_sql()
        with self.env.db_query as db:
            like = db.like()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.keywords AS keywords,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE ((NOT (COALESCE(t.keywords,'') %(like)s AND COALESCE(t.keywords,'') NOT %(like)s AND COALESCE(t.keywords,'') %(like)s)))
ORDER BY COALESCE(t.id,0)=0,t.id""" % {'like': like})
        self.assertEqual(['%foo%', '%bar%', '%baz%'], args)
        tickets = query.execute(self.req)
        self.assertEqual(['', '', 'foo', 'bar', 'baz', 'foo bar', 'bar baz',
                          'foo bar baz', ''],
                         [t['keywords'] for t in tickets])

    def test_constrained_by_milestone_or_version(self):
        query = Query.from_string(self.env, 'milestone=milestone1&or&version=version1', order='id')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.component AS component,t.time AS time,t.changetime AS changetime,t.version AS version,t.milestone AS milestone,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE ((COALESCE(t.milestone,'')=%s)) OR ((COALESCE(t.version,'')=%s))
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual(['milestone1', 'version1'], args)
        tickets = query.execute(self.req)
        self.assertEqual([('milestone1', '0.0'),
                          ('milestone2', 'version1'),
                          ('milestone1', ''),
                          ('',           'version1')],
                         [(t['milestone'], t['version']) for t in tickets])

    def test_equal_in_value(self):
        query = Query.from_string(self.env, r'status=this=that&version=version1',
                                  order='id')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.priority AS priority,t.milestone AS milestone,t.component AS component,t.status AS status,t.time AS time,t.changetime AS changetime,t.version AS version,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE ((COALESCE(t.status,'')=%s) AND (COALESCE(t.version,'')=%s))
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual(['this=that', 'version1'], args)
        tickets = query.execute(self.req)

    def test_special_character_escape(self):
        query = Query.from_string(self.env, r'status=here\&now|maybe\|later|back\slash',
                                  order='id')
        sql, args = query.get_sql()
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.status AS status,t.owner AS owner,t.type AS type,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE (COALESCE(t.status,'') IN (%s,%s,%s))
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual(['here&now', 'maybe|later', 'back\\slash'], args)
        tickets = query.execute(self.req)

    def test_repeated_constraint_field(self):
        like_query = Query.from_string(self.env, 'owner!=someone|someone_else',
                                       order='id')
        query = Query.from_string(self.env, 'owner!=someone&owner!=someone_else',
                                  order='id')
        like_sql, like_args = like_query.get_sql()
        sql, args = query.get_sql()
        self.assertEqualSQL(sql, like_sql)
        self.assertEqual(args, like_args)
        tickets = query.execute(self.req)

    def test_priority_value_in_custom_field(self):
        self.env.config.set('ticket-custom', 'priority_value', 'text')
        self._update_tickets('priority_value', [None, 'foo', 'bar', 'baz'])
        query = Query.from_string(self.env,
                                  'priority_value=baz&priority_value=foo')
        tickets = query.execute()
        self.assertEqual(set(['foo', 'baz']),
                         set(t['priority_value'] for t in tickets))
        self.assertIn(tickets[0]['_priority_value'],
                      (None, '1', '2', '3', '4', '5'))

    def test_user_var(self):
        query = Query.from_string(self.env, 'owner=$USER&order=id')
        sql, args = query.get_sql(req=self.req)
        self.assertEqualSQL(sql,
"""SELECT t.id AS id,t.summary AS summary,t.owner AS owner,t.type AS type,t.status AS status,t.priority AS priority,t.milestone AS milestone,t.time AS time,t.changetime AS changetime,priority.value AS _priority_value
FROM ticket AS t
  LEFT OUTER JOIN enum AS priority ON (priority.type='priority' AND priority.name=priority)
WHERE ((COALESCE(t.owner,'')=%s))
ORDER BY COALESCE(t.id,0)=0,t.id""")
        self.assertEqual(['anonymous'], args)
        tickets = query.execute(self.req)

    def test_csv_escape(self):
        query = Mock(get_columns=lambda: ['id', 'col1'],
                     execute=lambda r: [{'id': 1,
                                         'col1': 'value, needs escaped'}],
                     time_fields=['time', 'changetime'])
        req = Mock(href=self.env.href, perm=MockPerm())
        content, mimetype, ext = Mimeview(self.env).convert_content(
            req, 'trac.ticket.Query', query, 'csv')
        self.assertEqual('\xef\xbb\xbfid,col1\r\n1,"value, needs escaped"\r\n',
                         content)

    def test_csv_obfuscation(self):
        class NoEmailView(MockPerm):
            def has_permission(self, action, realm_or_resource=None, id=False,
                               version=False):
                return action != 'EMAIL_VIEW'
            __contains__ = has_permission

        query = Mock(get_columns=lambda: ['id', 'owner', 'reporter', 'cc'],
                     execute=lambda r: [{'id': 1,
                                         'owner': 'joe@example.org',
                                         'reporter': 'foo@example.org',
                                         'cc': 'cc1@example.org, cc2'}],
                     time_fields=['time', 'changetime'])
        req = Mock(href=self.env.href, perm=NoEmailView())
        content, mimetype, ext = Mimeview(self.env).convert_content(
            req, 'trac.ticket.Query', query, 'csv')
        self.assertEqual(u'\uFEFFid,owner,reporter,cc\r\n'
                         u'1,joe@…,foo@…,"cc1@…, cc2"\r\n',
                         content.decode('utf-8'))
        req = Mock(href=self.env.href, perm=MockPerm())
        content, mimetype, ext = Mimeview(self.env).convert_content(
            req, 'trac.ticket.Query', query, 'csv')
        self.assertEqual(
            u'\uFEFFid,owner,reporter,cc\r\n'
            u'1,joe@example.org,foo@example.org,"cc1@example.org, cc2"\r\n',
            content.decode('utf-8'))

    def test_template_data(self):
        req = Mock(href=self.env.href, perm=MockPerm(), authname='anonymous',
                   tz=None, locale=None)
        context = web_context(req, 'query')

        query = Query.from_string(self.env, 'owner=$USER&order=id')
        tickets = query.execute(req)
        data = query.template_data(context, tickets, req=req)
        self.assertEqual(['anonymous'], data['clauses'][0]['owner']['values'])

        query = Query.from_string(self.env, 'owner=$USER&order=id')
        tickets = query.execute(req)
        data = query.template_data(context, tickets)
        self.assertEqual(['$USER'], data['clauses'][0]['owner']['values'])


class QueryLinksTestCase(unittest.TestCase):

    def setUp(self):
        self.env = EnvironmentStub(default_data=True)
        self.query_module = QueryModule(self.env)
        self.req = Mock(perm=MockPerm(), args={}, arg_list=(), href=Href('/'),
                        authname='anonymous', chrome={}, session={}, tz=utc,
                        locale=None, lc_time=None, path_info='/query')
        self.context = web_context(self.req)
        self.formatter = LinkFormatter(self.env, self.context)

    def tearDown(self):
        self.env.reset_db()

    def _insert_ticket(self, **attrs):
        attrs.setdefault('reporter', 'joe')
        attrs.setdefault('summary', 'Summary')
        attrs.setdefault('status', 'new')
        ticket = Ticket(self.env)
        for name, value in attrs.iteritems():
            ticket[name] = value
        ticket.insert()
        return ticket

    def _format_link(self, query, label):
        return str(self.query_module._format_link(self.formatter, 'query',
                                                  query, label))

    def test_empty_query(self):
        self.assertEqual(self._format_link('', 'label'),
                         '<em class="error">[Error: Query filter requires '
                         'field and constraints separated by a "="]</em>')

    def _process_request(self, query_string):
        self.req.arg_list = parse_arg_list(query_string)
        self.req.args = arg_list_to_args(self.req.arg_list)
        self.assertEqual(True, self.query_module.match_request(self.req))
        template, data, content_type = \
                self.query_module.process_request(self.req)
        return data

    def test_duplicated_order_arguments(self):
        data = self._process_request('order=priority&order=id')
        self.assertEqual([], data['tickets'])
        self.assertEqual('priority', data['query'].order)

    def test_duplicated_report_arguments(self):
        data = self._process_request('report=1&report=2')
        self.assertEqual([], data['tickets'])
        self.assertEqual('1', data['query'].id)

    def test_duplicated_group_arguments(self):
        self._insert_ticket(status='new')
        self._insert_ticket(status='assigned')
        data = self._process_request(
                'group=status&group=status&order=priority')
        self.assertNotEqual([], data['tickets'])
        self.assertEqual(set(('new', 'assigned')),
                         set(t['status'] for t in data['tickets']))
        self.assertEqual(2, len(data['tickets']))
        self.assertNotEqual([], data['groups'])
        self.assertEqual(set(('new', 'assigned')),
                         set(value for value, tickets in data['groups']))
        self.assertEqual(2, len(data['groups']))


class TicketQueryMacroTestCase(unittest.TestCase):

    def assertQueryIs(self, content, query, kwargs, format):
        qs, kw, f = TicketQueryMacro.parse_args(content)
        self.assertEqual(query, qs)
        self.assertEqual(kwargs, kw)
        self.assertEqual(format, f)

    def test_owner_and_milestone(self):
        self.assertQueryIs('owner=joe, milestone=milestone1',
                           'owner=joe&milestone=milestone1',
                           dict(col='status|summary', max='0', order='id'),
                           'list')

    def test_owner_or_milestone(self):
        self.assertQueryIs('owner=joe, or, milestone=milestone1',
                           'owner=joe&or&milestone=milestone1',
                           dict(col='status|summary', max='0', order='id'),
                           'list')

    def test_format_arguments(self):
        self.assertQueryIs('owner=joe, milestone=milestone1, col=component|severity, max=15, order=component, format=compact',
                           'owner=joe&milestone=milestone1',
                           dict(col='status|summary|component|severity', max='15', order='component'),
                           'compact')
        self.assertQueryIs('owner=joe, milestone=milestone1, col=id|summary|component, max=30, order=component, format=table',
                           'owner=joe&milestone=milestone1',
                           dict(col='id|summary|component', max='30', order='component'),
                           'table')

    def test_special_char_escaping(self):
        self.assertQueryIs(r'owner=joe|jack, milestone=this\&that\|here\,now',
                           r'owner=joe|jack&milestone=this\&that\|here,now',
                           dict(col='status|summary', max='0', order='id'),
                           'list')

QUERY_TEST_CASES = u"""

============================== TicketQuery
[[TicketQuery]]
------------------------------
<p>
</p><div>\
<dl class="wiki compact">\
<dt><a class="new" href="/ticket/1" title="This is the summary">#1</a></dt>\
<dd>This is the summary</dd>\
<dt><a class="assigned" href="/ticket/2" title="This is another summary">#2</a></dt>\
<dd>This is another summary</dd>\
<dt><a class="closed" href="/ticket/3" title="This is th third summary">#3</a></dt>\
<dd>This is th third summary</dd>\
</dl>\
</div><p>
</p>
------------------------------
============================== TicketQuery()
[[TicketQuery()]]
------------------------------
<p>
</p><div>\
<dl class="wiki compact">\
<dt><a class="new" href="/ticket/1" title="This is the summary">#1</a></dt>\
<dd>This is the summary</dd>\
<dt><a class="assigned" href="/ticket/2" title="This is another summary">#2</a></dt>\
<dd>This is another summary</dd>\
<dt><a class="closed" href="/ticket/3" title="This is th third summary">#3</a></dt>\
<dd>This is th third summary</dd>\
</dl>\
</div><p>
</p>
------------------------------
============================== TicketQuery(created=...)
[[TicketQuery(created=...)]]
------------------------------
<p>
</p><div class="system-message"><strong>Error executing TicketQuery macro</strong><pre>Invalid query constraint value</pre></div><p>
</p>
------------------------------
============================== TicketQuery(format=progress)
[[TicketQuery(format=progress)]]
------------------------------
<p>
</p><div class="trac-progress">

  <table xmlns="http://www.w3.org/1999/xhtml" class="progress">
    <tr>
      <td class="closed" style="width: 33%">
        <a href="/query?status=closed&amp;group=resolution&amp;max=0&amp;order=time" title="1/3 closed"></a>
      </td><td class="open" style="width: 67%">
        <a href="/query?status=assigned&amp;status=new&amp;status=accepted&amp;status=reopened&amp;max=0&amp;order=id" title="2/3 active"></a>
      </td>
    </tr>
  </table>

  <p class="percent">33%</p>

  <p class="legend">
    <span class="first interval">
      <a href="/query?max=0&amp;order=id">Total number of tickets: 3</a>
    </span>
    <span class="interval">
      - <a href="/query?status=closed&amp;group=resolution&amp;max=0&amp;order=time">closed: 1</a>
    </span><span class="interval">
      - <a href="/query?status=assigned&amp;status=new&amp;status=accepted&amp;status=reopened&amp;max=0&amp;order=id">active: 2</a>
    </span>
  </p>
</div><p>
</p>
------------------------------
============================== TicketQuery(reporter=santa, format=progress)
[[TicketQuery(reporter=santa, format=progress)]]
------------------------------
<p>
</p><div class="trac-progress">

  <table xmlns="http://www.w3.org/1999/xhtml" class="progress">
    <tr>
      <td class="closed" style="display: none">
        <a href="/query?status=closed&amp;reporter=santa&amp;group=resolution&amp;max=0&amp;order=time" title="0/1 closed"></a>
      </td><td class="open" style="width: 100%">
        <a href="/query?status=assigned&amp;status=new&amp;status=accepted&amp;status=reopened&amp;reporter=santa&amp;max=0&amp;order=id" title="1/1 active"></a>
      </td>
    </tr>
  </table>

  <p class="percent">0%</p>

  <p class="legend">
    <span class="first interval">
      <a href="/query?reporter=santa&amp;max=0&amp;order=id">Total number of tickets: 1</a>
    </span>
    <span class="interval">
      - <a href="/query?status=closed&amp;reporter=santa&amp;group=resolution&amp;max=0&amp;order=time">closed: 0</a>
    </span><span class="interval">
      - <a href="/query?status=assigned&amp;status=new&amp;status=accepted&amp;status=reopened&amp;reporter=santa&amp;max=0&amp;order=id">active: 1</a>
    </span>
  </p>
</div><p>
</p>
------------------------------
============================== TicketQuery(reporter=santa&or&owner=santa, format=progress)
[[TicketQuery(reporter=santa&or&owner=santa, format=progress)]]
------------------------------
<p>
</p><div class="trac-progress">

  <table xmlns="http://www.w3.org/1999/xhtml" class="progress">
    <tr>
      <td class="closed" style="width: 50%">
        <a href="/query?status=closed&amp;reporter=santa&amp;or&amp;owner=santa&amp;status=closed&amp;group=resolution&amp;max=0&amp;order=time" title="1/2 closed"></a>
      </td><td class="open" style="width: 50%">
        <a href="/query?status=assigned&amp;status=new&amp;status=accepted&amp;status=reopened&amp;reporter=santa&amp;or&amp;owner=santa&amp;status=assigned&amp;status=new&amp;status=accepted&amp;status=reopened&amp;max=0&amp;order=id" title="1/2 active"></a>
      </td>
    </tr>
  </table>

  <p class="percent">50%</p>

  <p class="legend">
    <span class="first interval">
      <a href="/query?reporter=santa&amp;or&amp;owner=santa&amp;max=0&amp;order=id">Total number of tickets: 2</a>
    </span>
    <span class="interval">
      - <a href="/query?status=closed&amp;reporter=santa&amp;or&amp;owner=santa&amp;status=closed&amp;group=resolution&amp;max=0&amp;order=time">closed: 1</a>
    </span><span class="interval">
      - <a href="/query?status=assigned&amp;status=new&amp;status=accepted&amp;status=reopened&amp;reporter=santa&amp;or&amp;owner=santa&amp;status=assigned&amp;status=new&amp;status=accepted&amp;status=reopened&amp;max=0&amp;order=id">active: 1</a>
    </span>
  </p>
</div><p>
</p>
------------------------------
============================== TicketQuery(format=progress, group=project)
[[TicketQuery(format=progress, group=project)]]
------------------------------
<p>
</p><div class="trac-groupprogress">
  <table xmlns="http://www.w3.org/1999/xhtml" summary="Ticket completion status for each project">
    <tr>
      <th scope="row">
        <i><a href="/query?project=&amp;max=0&amp;order=id">(none)</a></i>


      </th>
      <td>


  <table class="progress" style="width: 40%">
    <tr>
      <td class="closed" style="display: none">
        <a href="/query?project=&amp;status=closed&amp;group=resolution&amp;max=0&amp;order=time" title="0/1 closed"></a>
      </td><td class="open" style="width: 100%">
        <a href="/query?project=&amp;status=assigned&amp;status=new&amp;status=accepted&amp;status=reopened&amp;max=0&amp;order=id" title="1/1 active"></a>
      </td>
    </tr>
  </table>

  <p class="percent">0 / 1</p>



      </td>
    </tr><tr>
      <th scope="row">


        <a href="/query?project=xmas&amp;max=0&amp;order=id">xmas</a>
      </th>
      <td>


  <table class="progress" style="width: 80%">
    <tr>
      <td class="closed" style="width: 50%">
        <a href="/query?project=xmas&amp;status=closed&amp;group=resolution&amp;max=0&amp;order=time" title="1/2 closed"></a>
      </td><td class="open" style="width: 50%">
        <a href="/query?project=xmas&amp;status=assigned&amp;status=new&amp;status=accepted&amp;status=reopened&amp;max=0&amp;order=id" title="1/2 active"></a>
      </td>
    </tr>
  </table>

  <p class="percent">1 / 2</p>



      </td>
    </tr>
  </table>
</div><p>
</p>
------------------------------
============================== TicketQuery(reporter=santa, format=progress, group=project)
[[TicketQuery(reporter=santa, format=progress, group=project)]]
------------------------------
<p>
</p><div class="trac-groupprogress">
  <table xmlns="http://www.w3.org/1999/xhtml" summary="Ticket completion status for each project">
    <tr>
      <th scope="row">


        <a href="/query?project=xmas&amp;reporter=santa&amp;max=0&amp;order=id">xmas</a>
      </th>
      <td>


  <table class="progress" style="width: 80%">
    <tr>
      <td class="closed" style="display: none">
        <a href="/query?project=xmas&amp;status=closed&amp;reporter=santa&amp;group=resolution&amp;max=0&amp;order=time" title="0/1 closed"></a>
      </td><td class="open" style="width: 100%">
        <a href="/query?project=xmas&amp;status=assigned&amp;status=new&amp;status=accepted&amp;status=reopened&amp;reporter=santa&amp;max=0&amp;order=id" title="1/1 active"></a>
      </td>
    </tr>
  </table>

  <p class="percent">0 / 1</p>



      </td>
    </tr>
  </table>
</div><p>
</p>
------------------------------
============================== TicketQuery(reporter=santa&or&owner=santa, format=progress, group=project)
[[TicketQuery(reporter=santa&or&owner=santa, format=progress, group=project)]]
------------------------------
<p>
</p><div class="trac-groupprogress">
  <table xmlns="http://www.w3.org/1999/xhtml" summary="Ticket completion status for each project">
    <tr>
      <th scope="row">


        <a href="/query?project=xmas&amp;reporter=santa&amp;or&amp;owner=santa&amp;project=xmas&amp;max=0&amp;order=id">xmas</a>
      </th>
      <td>


  <table class="progress" style="width: 80%">
    <tr>
      <td class="closed" style="width: 50%">
        <a href="/query?project=xmas&amp;status=closed&amp;reporter=santa&amp;or&amp;owner=santa&amp;project=xmas&amp;status=closed&amp;group=resolution&amp;max=0&amp;order=time" title="1/2 closed"></a>
      </td><td class="open" style="width: 50%">
        <a href="/query?project=xmas&amp;status=assigned&amp;status=new&amp;status=accepted&amp;status=reopened&amp;reporter=santa&amp;or&amp;owner=santa&amp;project=xmas&amp;status=assigned&amp;status=new&amp;status=accepted&amp;status=reopened&amp;max=0&amp;order=id" title="1/2 active"></a>
      </td>
    </tr>
  </table>

  <p class="percent">1 / 2</p>



      </td>
    </tr>
  </table>
</div><p>
</p>
------------------------------
"""

def ticket_setup(tc):
    tc.env.config.set('ticket-custom', 'project', 'text')
    ticket = Ticket(tc.env)
    ticket.populate({'reporter': 'santa',
                     'summary': 'This is the summary',
                     'status': 'new',
                     'project': 'xmas'})
    ticket.insert()
    ticket = Ticket(tc.env)
    ticket.populate({'owner': 'elf',
                     'summary': 'This is another summary',
                     'status': 'assigned'})
    ticket.insert()
    ticket = Ticket(tc.env)
    ticket.populate({'owner': 'santa',
                     'summary': 'This is th third summary',
                     'status': 'closed',
                     'project': 'xmas'})
    ticket.insert()

    tc.env.config.set('milestone-groups', 'closed.status', 'closed')
    tc.env.config.set('milestone-groups', 'closed.query_args', 'group=resolution,order=time')
    tc.env.config.set('milestone-groups', 'closed.overall_completion', 'true')
    tc.env.config.set('milestone-groups', 'active.status', '*')
    tc.env.config.set('milestone-groups', 'active.css_class', 'open')

def ticket_teardown(tc):
    tc.env.reset_db()

def suite():
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(QueryTestCase))
    suite.addTest(unittest.makeSuite(QueryLinksTestCase))
    suite.addTest(unittest.makeSuite(TicketQueryMacroTestCase))
    suite.addTest(formatter.suite(QUERY_TEST_CASES, ticket_setup, __file__,
                                  ticket_teardown))
    return suite

if __name__ == '__main__':
    unittest.main(defaultTest='suite')
