import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { environment } from '../../../environments/environment';
import { Rule, ServiceActionType  } from '../../pages/notifications/rule';
import { RulesService, RuleResponse } from './rules.service';

const NOTIFIER_URL = environment.notifier_url;

describe('RulesService', () => {
  let service: RulesService;
  let httpTestingController: HttpTestingController;

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        RulesService
      ],
      imports: [
        HttpClientTestingModule
      ]
    });
    service = TestBed.get(RulesService);
    httpTestingController = TestBed.get(HttpTestingController);
  });

  describe('fetchRules', () => {
    it('calls get with the correct url', () => {
      const expectedUrl = `${NOTIFIER_URL}/rules`;

      service.fetchRules().subscribe((resp) => expect(resp).toEqual([]));

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('GET');

      req.flush([]);
    });
  });

  describe('deleteRule', () => {
    it('calls delete with the correct url', () => {
      const rule = new Rule('1234', '', null, '', ServiceActionType.SLACK, '', false);
      const expectedUrl = `${NOTIFIER_URL}/rules/${rule.id}`;
      const response = <RuleResponse>{ rule: {} };

      service.deleteRule(rule).subscribe((resp) => expect(resp).toEqual(response));

      const req = httpTestingController.expectOne(expectedUrl);
      expect(req.request.method).toEqual('DELETE');
      req.flush(response);
    });
  });

  describe('editRule', () => {
    it('calls put with the correct url', () => {
      const rule = new Rule('1234', '', null, '', ServiceActionType.SLACK, '', false);
      const expectedUrl = `${NOTIFIER_URL}/rules/${rule.id}`;
      const response = <RuleResponse>{ rule: {} };

      service.editRule(rule.id, rule, '', '').subscribe((resp) => expect(resp).toEqual(response));

      const req = httpTestingController.expectOne(expectedUrl);
      expect(req.request.method).toEqual('PUT');
      req.flush(response);
    });

    it('calls put and patch with the correct url', () => {
      const secretId = '5678';
      const rule = new Rule('1234', '', null, '', ServiceActionType.SERVICENOW, secretId, false);
      const expectedUrl = `${NOTIFIER_URL}/rules/${rule.id}`;
      const response = <RuleResponse>{ rule: {} };

      service.editRule(rule.id, rule, 'bob', 'super_secret_password').
        subscribe((resp) => expect(resp).toEqual(response));

      const secretsReq = httpTestingController.
        expectOne(`${environment.secrets_url}/id/${secretId}`);
      expect(secretsReq.request.method).toEqual('PATCH');
      secretsReq.flush({});

      const req = httpTestingController.expectOne(expectedUrl);
      expect(req.request.method).toEqual('PUT');
      req.flush(response);
    });

    it('calls put with the correct url when not username or password is set', () => {
      const secretId = '5678';
      const rule = new Rule('1234', '', null, '', ServiceActionType.SERVICENOW, secretId, false);
      const expectedUrl = `${NOTIFIER_URL}/rules/${rule.id}`;
      const response = <RuleResponse>{ rule: {} };

      service.editRule(rule.id, rule, '', '').
        subscribe((resp) => expect(resp).toEqual(response));

      const req = httpTestingController.expectOne(expectedUrl);
      expect(req.request.method).toEqual('PUT');
      expect(Rule.fromResponse(req.request.body.rule).targetSecretId).toEqual(secretId);
      req.flush(response);
    });
  });

  describe('createRule', () => {
    it('calls post with the correct url', () => {
      const expectedUrl = `${NOTIFIER_URL}/rules`;
      const rule = new Rule('', '', null, '', ServiceActionType.SLACK, '', false);
      const response = <RuleResponse>{ rule: {} };

      service.createRule(rule, '', '').subscribe((resp) => expect(resp).toEqual(response));

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('POST');

      req.flush(response);
    });

    it('with username and password, calls post with the correct url', () => {
      const rule = new Rule('', '', null, '', ServiceActionType.SERVICENOW, 'super_secret_id', false);
      const response = <RuleResponse>{ rule: {} };

      service.createRule(rule, 'bob', 'super_secret_password').
        subscribe((resp) => expect(resp).toEqual(response));

      const secretsReq = httpTestingController.expectOne(environment.secrets_url);
      expect(secretsReq.request.method).toEqual('POST');
      secretsReq.flush({});

      const notifierReq = httpTestingController.expectOne(`${NOTIFIER_URL}/rules`);
      expect(notifierReq.request.method).toEqual('POST');
      notifierReq.flush(response);
    });
  });
});
