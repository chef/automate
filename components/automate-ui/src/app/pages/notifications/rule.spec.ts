import { Rule, ServiceActionType } from './rule';

describe('Rule', () => {
  const URL = 'http://example.com';

  describe('fromResponse', () => {

    it('can parse a slack rule with CCRFailure', () => {
      const rule = { id: 'myid', name: 'foo', event: 'CCRFailure',
         SlackAlert: { url: URL} };
      const expected = new Rule('myid', 'foo', 'CCRFailure',
        URL, ServiceActionType.SLACK, '', false);
      expect(Rule.fromResponse(rule)).toEqual(expected);
    });

    it('can parse a slack rule with ComplianceFailure', () => {
      const rule = { id: 'myid', name: 'foo', event: 'ComplianceFailure',
        SlackAlert: { url: URL} };
      const expected = new Rule('myid', 'foo', 'ComplianceFailure',
        URL, ServiceActionType.SLACK, '', false);
      expect(Rule.fromResponse(rule)).toEqual(expected);
    });

    it('can parse a webhook rule with CCRFailure', () => {
      const rule = { id: 'myid', name: 'foo', event: 'CCRFailure',
        WebhookAlert: { url: URL} } ;
      const expected = new Rule('myid', 'foo', 'CCRFailure',
        URL, ServiceActionType.WEBHOOK, '', false);
      expect(Rule.fromResponse(rule)).toEqual(expected);
    });

    it('can parse a webhook rule with ComplianceFailure', () => {
      const rule = { id: 'myid', name: 'foo', event: 'ComplianceFailure',
        WebhookAlert: { url: URL} };
      const expected = new Rule('myid', 'foo', 'ComplianceFailure',
        URL, ServiceActionType.WEBHOOK, '', false);
      expect(Rule.fromResponse(rule)).toEqual(expected);
    });

    it('can parse a servicenow rule with CCRFailure', () => {
      const rule = { id: 'myid', name: 'foo', event: 'CCRFailure',
        [ServiceActionType.SERVICENOW]: { url: URL, secret_id: 'super_secret_id',
        critical_controls_only: false}} ;
      const expected = new Rule('myid', 'foo', 'CCRFailure',
        URL, ServiceActionType.SERVICENOW, 'super_secret_id', false);
      expect(Rule.fromResponse(rule)).toEqual(expected);
    });

    it('can parse a servicenow rule with ComplianceFailure', () => {
      const rule = { id: 'myid', name: 'foo', event: 'ComplianceFailure',
        [ServiceActionType.SERVICENOW]: { url: URL, secret_id: 'super_secret_id',
        critical_controls_only: true}};
      const expected = new Rule('myid', 'foo', 'ComplianceFailure',
        URL, ServiceActionType.SERVICENOW, 'super_secret_id', true);
      expect(Rule.fromResponse(rule)).toEqual(expected);
    });
  });

  describe('toRequest', () => {
    it('can convert a slack rule with CCRFailure', () => {
      const rule = new Rule('myid', 'foo', 'CCRFailure', URL, ServiceActionType.SLACK, '', false);
      const expected = { rule: { name: 'foo', event: 'CCRFailure',
        SlackAlert: { url: URL } } };
      expect(rule.toRequest()).toEqual(expected);
    });

    it('can convert a slack rule with ComplianceFailure', () => {
      const rule = new Rule('myid', 'foo', 'ComplianceFailure',
        URL, ServiceActionType.SLACK, '', false);
      const expected = { rule: { name: 'foo', event: 'ComplianceFailure',
        SlackAlert: { url: URL } } };
      expect(rule.toRequest()).toEqual(expected);
    });

    it('can convert a webhook rule with CCRFailure', () => {
      const rule = new Rule('myid', 'foo', 'CCRFailure', URL,
        ServiceActionType.WEBHOOK, '', false);
      const expected = { rule: { name: 'foo', event: 'CCRFailure',
        WebhookAlert: { url: URL } } };
      expect(rule.toRequest()).toEqual(expected);
    });

    it('can convert a webhook rule with ComplianceFailure', () => {
      const rule = new Rule('myid', 'foo', 'ComplianceFailure', URL,
        ServiceActionType.WEBHOOK, '', false);
      const expected = { rule: { name: 'foo', event: 'ComplianceFailure',
        WebhookAlert: { url: URL } }  };
      expect(rule.toRequest()).toEqual(expected);
    });

    it('can convert a servicenow rule with CCRFailure', () => {
      const rule = new Rule('myid', 'foo', 'CCRFailure', URL,
        ServiceActionType.SERVICENOW, 'super_secret_id', false);
      const expected = { rule: { name: 'foo', event: 'CCRFailure',
        [ServiceActionType.SERVICENOW]: { url: URL,
          secret_id: 'super_secret_id', critical_controls_only: false}}};
      expect(rule.toRequest()).toEqual(expected);
    });

    it('can convert a servicenow rule with ComplianceFailure', () => {
      const rule = new Rule('myid', 'foo', 'ComplianceFailure', URL,
        ServiceActionType.SERVICENOW, 'super_secret_id', true);
      const expected = { rule: { name: 'foo', event: 'ComplianceFailure',
        [ServiceActionType.SERVICENOW]: { url: URL,
          secret_id: 'super_secret_id', critical_controls_only: true}}};
      expect(rule.toRequest()).toEqual(expected);
    });
  });
});
