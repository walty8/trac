# -*- coding: utf-8 -*-
#
# Copyright (C) 2010 Brian Meeker
# All rights reserved.
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at http://trac.edgewall.org/wiki/TracLicense.
#
# This software consists of voluntary contributions made by many
# individuals. For the exact contribution history, see the revision
# history and logs, available at http://trac.edgewall.org/log/.
#
# Author: Brian Meeker <meeker.brian@gmail.com>

import re

from genshi.builder import tag

from trac.core import *
from trac.notification.api import NotificationSystem
from trac.perm import IPermissionRequestor
from trac.ticket import TicketSystem, Ticket
from trac.ticket.notification import BatchTicketChangeEvent
from trac.ticket.api import ITicketManipulator
from trac.util.datefmt import datetime_now, parse_date, user_time, utc
from trac.util.text import exception_to_unicode, to_unicode
from trac.util.translation import _, tag_
from trac.web import IRequestHandler
from trac.web.chrome import add_warning, add_script_data


class BatchModifyModule(Component):
    """Ticket batch modification module.

    This component allows multiple tickets to be modified in one request from
    the custom query page. For users with the TICKET_BATCH_MODIFY permission
    it will add a [TracBatchModify batch modify] section underneath custom
    query results. Users can choose which tickets and fields they wish to
    modify.
    """

    implements(IPermissionRequestor, IRequestHandler)

    ticket_manipulators = ExtensionPoint(ITicketManipulator)
    is_valid_default_handler = False

    list_separator_re = re.compile(r'[;\s,]+')
    list_connector_string = ', '

    # IRequestHandler methods

    def match_request(self, req):
        return req.path_info == '/batchmodify'

    def process_request(self, req):
        req.perm.assert_permission('TICKET_BATCH_MODIFY')

        comment = req.args.get('batchmod_value_comment', '')
        action = req.args.get('action')

        try:
            new_values = self._get_new_ticket_values(req)
        except TracError as e:
            new_values = None
            add_warning(req, tag_("The changes could not be saved: "
                                  "%(message)s", message=to_unicode(e)))

        if new_values is not None:
            selected_tickets = self._get_selected_tickets(req)
            if self._validate_tickets(req, selected_tickets, new_values):
                self._save_ticket_changes(req, selected_tickets,
                                          new_values, comment, action)

        # Always redirect back to the query page we came from.
        req.redirect(req.session['query_href'])

    # IPermissionRequestor methods

    def get_permission_actions(self):
        return ['TICKET_BATCH_MODIFY',
                ('TICKET_ADMIN', ['TICKET_BATCH_MODIFY'])]

    def _get_new_ticket_values(self, req):
        """Pull all of the new values out of the post data."""
        values = {}

        for field in TicketSystem(self.env).get_ticket_fields():
            name = field['name']
            if name not in ('id', 'resolution', 'status', 'owner', 'time',
                            'changetime', 'summary', 'description') + \
                           (('reporter',) if 'TICKET_ADMIN' not in req.perm
                                          else ()) \
                    and field['type'] != 'textarea':
                value = req.args.get('batchmod_value_' + name)
                if value is not None:
                    values[name] = self._parse_field_value(req, field, value)
        return values

    def _parse_field_value(self, req, field, value):
        if field['type'] == 'time':
            return user_time(req, parse_date, value)
        return value

    def _get_selected_tickets(self, req):
        """The selected tickets will be a comma separated list
        in the request arguments."""
        selected_tickets = req.args.get('selected_tickets')
        if selected_tickets == '':
            return []
        else:
            return selected_tickets.split(',')

    def add_template_data(self, req, data, tickets):
        data['batch_modify'] = True
        data['query_href'] = req.session['query_href'] or req.href.query()
        data['action_controls'] = self._get_action_controls(req, tickets)
        batch_list_modes = [
            {'name': _("add"), 'value': "+"},
            {'name': _("remove"), 'value': "-"},
            {'name': _("add / remove"), 'value': "+-"},
            {'name': _("set to"), 'value': "="},
        ]
        add_script_data(req, batch_list_modes=batch_list_modes,
                        batch_list_properties=self._get_list_fields())

    def _get_list_fields(self):
        return [f['name']
                for f in TicketSystem(self.env).get_ticket_fields()
                if f['type'] == 'text' and f.get('format') == 'list']

    def _get_action_controls(self, req, tickets):
        action_controls = []
        ts = TicketSystem(self.env)
        tickets_by_action = {}
        for t in tickets:
            ticket = Ticket(self.env, t['id'])
            actions = ts.get_available_actions(req, ticket)
            for action in actions:
                tickets_by_action.setdefault(action, []).append(ticket)
        sorted_actions = sorted(set(tickets_by_action.keys()))
        for action in sorted_actions:
            first_label = None
            hints = []
            widgets = []
            ticket = tickets_by_action[action][0]
            for controller in self._get_action_controllers(req, ticket,
                                                           action):
                label, widget, hint = controller.render_ticket_action_control(
                    req, ticket, action)
                if not first_label:
                    first_label = label
                widgets.append(widget)
                hints.append(hint)
            action_controls.append((action, first_label, tag(widgets), hints))
        return action_controls

    def _get_action_controllers(self, req, ticket, action):
        """Generator yielding the controllers handling the given `action`"""
        for controller in TicketSystem(self.env).action_controllers:
            actions = [a for w, a in
                       controller.get_ticket_actions(req, ticket) or []]
            if action in actions:
                yield controller

    def _get_updated_ticket_values(self, req, ticket, new_values):
        list_fields = self._get_list_fields()
        _values = new_values.copy()
        for field in list_fields:
            if field in new_values:
                old = ticket[field] if field in ticket else ''
                new = new_values[field]
                mode = req.args.get('batchmod_value_' + field +
                                    '_mode')
                new2 = req.args.get('batchmod_value_' + field +
                                    '_secondary', '')
                _values[field] = self._change_list(old, new, new2,
                                                   mode)
        return _values


    def _validate_tickets(self, req, selected_tickets, new_values):
        valid = True
        for id in selected_tickets:
            ticket = Ticket(self.env, int(id))
            _values = self._get_updated_ticket_values(req, ticket, 
                                                      new_values)

            for field_name, field_value in _values.items():
                ticket[field_name] = field_value

            for manipulator in self.ticket_manipulators:
                for field, message in manipulator.validate_ticket(req, 
                                                                  ticket):
                    valid = False
                    if field:
                        add_warning(req, tag_("The field %(field)s"
                                              " for ticket #%(ticketId)s"
                                              " is invalid: %(message)s",
                                              field=tag.strong(field),
                                              ticketId=id,
                                              message=message))
                    else:
                        add_warning(req, message)
        return valid


    def _save_ticket_changes(self, req, selected_tickets,
                             new_values, comment, action):
        """Save all of the changes to tickets."""
        when = datetime_now(utc)
        with self.env.db_transaction as db:
            for id in selected_tickets:
                t = Ticket(self.env, int(id))
                _values = self._get_updated_ticket_values(req, t, new_values)
                controllers = list(self._get_action_controllers(req, t,
                                                                action))
                for controller in controllers:
                    _values.update(controller.get_ticket_changes(req, t,
                                                                 action))
                t.populate(_values)
                t.save_changes(req.authname, comment, when=when)
                for controller in controllers:
                    controller.apply_action_side_effects(req, t, action)
        event = BatchTicketChangeEvent(selected_tickets, when,
                                       req.authname, comment, new_values,
                                       action)
        try:
            NotificationSystem(self.env).notify(event)
        except Exception as e:
            self.log.error("Failure sending notification on ticket batch"
                           "change: %s", exception_to_unicode(e))
            add_warning(req, tag_("The changes have been saved, but an "
                                  "error occurred while sending "
                                  "notifications: %(message)s",
                                  message=to_unicode(e)))

    def _change_list(self, old_list, new_list, new_list2, mode):
        changed_list = [k.strip()
                        for k in self.list_separator_re.split(old_list)
                        if k]
        new_list = [k.strip()
                    for k in self.list_separator_re.split(new_list)
                    if k]
        new_list2 = [k.strip()
                     for k in self.list_separator_re.split(new_list2)
                     if k]

        if mode == '=':
            changed_list = new_list
        elif mode == '+':
            for entry in new_list:
                if entry not in changed_list:
                    changed_list.append(entry)
        elif mode == '-':
            for entry in new_list:
                while entry in changed_list:
                    changed_list.remove(entry)
        elif mode == '+-':
            for entry in new_list:
                if entry not in changed_list:
                    changed_list.append(entry)
            for entry in new_list2:
                while entry in changed_list:
                    changed_list.remove(entry)
        return self.list_connector_string.join(changed_list)
