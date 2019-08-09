import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { MockChefSessionService } from 'app/testing/mock-chef-session.service';
import { StatsService } from './stats.service';
import * as moment from 'moment';
import { environment } from '../../../../../environments/environment';

const COMPLIANCE_URL = environment.compliance_url;

describe('StatsService', () => {
  let httpTestingController: HttpTestingController;
  let service: StatsService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        { provide: ChefSessionService, useClass: MockChefSessionService },
        StatsService
      ],
      imports: [
        HttpClientTestingModule
      ]
    });

    service = TestBed.get(StatsService);
    httpTestingController = TestBed.get(HttpTestingController);
  });

  afterEach(() => {
    httpTestingController.verify();
  });

  describe('getNodes()', () => {
    it('fetches nodes data with filters', () => {
      const filters = [{type: {name: 'platform'}, value: {text: 'centos'}}];
      const listParams = {perPage: 10, page: 1};

      const expectedUrl = `${COMPLIANCE_URL}/reporting/nodes/search`;
      const expectedTotal = 20;
      const expectedTotalFailed = 1;
      const expectedTotalPassed = 18;
      const expectedTotalSkipped = 2;
      const expectedItems = [{}, {}];
      const mockResp = {
        nodes: expectedItems,
        total: expectedTotal,
        total_failed: expectedTotalFailed,
        total_passed: expectedTotalPassed,
        total_skipped: expectedTotalSkipped,
      };

      service.getNodes(filters, listParams).subscribe(data => {
        expect(data).toEqual({
          total: expectedTotal,
          items: expectedItems,
          total_failed: expectedTotalFailed,
          total_passed: expectedTotalPassed,
          total_skipped: expectedTotalSkipped,
        });
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('POST');

      req.flush(mockResp);
    });
  });

  describe('getProfiles()', () => {
    it('fetches profiles data', () => {
      const filters = [{type: {name: 'profile'}, value: {id: '456'}}];
      const listParams = {perPage: 10, page: 1};

      const expectedUrl = `${COMPLIANCE_URL}/reporting/profiles`;
      const expectedTotal = 20;
      const expectedItems = [{}, {}];
      const mockResp = { profiles: expectedItems, counts: { total: expectedTotal }};

      service.getProfiles(filters, listParams).subscribe(data => {
        expect(data).toEqual({total: expectedTotal, items: expectedItems});
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('POST');

      req.flush(mockResp);
    });
  });

  describe('getNodeSummary()', () => {
    it('fetches node summary data', () => {
      const filters = [{type: {name: 'profile'}, value: {id: '456'}}];

      const expectedUrl = `${COMPLIANCE_URL}/reporting/stats/summary`;
      const expectedData = {};
      const mockResp = {node_summary: expectedData};

      service.getNodeSummary(filters).subscribe(data => {
        expect(data).toEqual(expectedData);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('POST');

      req.flush(mockResp);
    });
  });

  describe('getControlsSummary()', () => {
    it('fetches controls summary data', () => {
      const filters = [{type: {name: 'profile'}, value: {id: '456'}}];

      const expectedUrl = `${COMPLIANCE_URL}/reporting/stats/summary`;
      const expectedData = {};
      const mockResp = {controls_summary: expectedData};

      service.getControlsSummary(filters).subscribe(data => {
        expect(data).toEqual(expectedData);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('POST');

      req.flush(mockResp);
    });
  });

  describe('formatFilters()', () => {
    it('returns the correct filter params', () => {
      expect(service.formatFilters([
        {type: {name: 'profile'}, value: {id: '123'}},
        {type: {name: 'profile'}, value: {id: '456'}},
        {type: {name: 'node'}, value: {id: '1223'}},
        {type: {name: 'platform'}, value: {text: 'centos'}},
        {type: {name: 'environment'}, value: {text: 'Dev Sec'}},
        {'end_time': moment('2017-11-15T23:59:59Z').utcOffset(0)},
        {'start_time': moment('2017-11-14T00:00:00Z').utcOffset(0)}
      ])).toEqual([
        {type: 'profile_id', values: ['123', '456']},
        {type: 'node_id', values: ['1223']},
        {type: 'platform', values: ['centos']},
        {type: 'environment', values: ['Dev Sec']},
        {type: 'end_time', values: ['2017-11-15T23:59:59Z']},
        {type: 'start_time', values: ['2017-11-14T00:00:00Z']}
      ]);
    });

    it('returns profile_name not profile_id', () => {
      expect(service.formatFilters([
        {type: {name: 'profile'}, value: {text: '123'}}
      ])).toEqual([
        {type: 'profile_name', values: ['123']}
      ]);
    });

    it('returns profile_id not profile_name, when there is no value id', () => {
      expect(service.formatFilters([
        {type: {name: 'profile'}, value: {id: '123', text: '456'}}
      ])).toEqual([
        {type: 'profile_id', values: ['123']}
      ]);
    });

    it('returns node_name not node_id', () => {
      expect(service.formatFilters([
        {type: {name: 'node'}, value: {text: '123'}}
      ])).toEqual([
        {type: 'node_name', values: ['123']}
      ]);
    });

    it('returns node_id not node_name, when there is no value id', () => {
      expect(service.formatFilters([
        {type: {name: 'node'}, value: {id: '123', text: '456'}}
      ])).toEqual([
        {type: 'node_id', values: ['123']}
      ]);
    });

    it('returns control_name not control', () => {
      expect(service.formatFilters([
        {type: {name: 'control'}, value: {text: '123'}}
      ])).toEqual([
        {type: 'control_name', values: ['123']}
      ]);
    });

    it('returns control not control_name, when there is no value id', () => {
      expect(service.formatFilters([
        {type: {name: 'control'}, value: {id: '123', text: '456'}}
      ])).toEqual([
        {type: 'control', values: ['123']}
      ]);
    });
  });

  describe('addDateRange', () => {
    it('adds date range end_time and start_time to the filters array', () => {
      const filters = [{'type': 'Profile', 'value': '456'}];
      const endDate = moment('2017-01-31T00:00:00Z').utcOffset(0);
      const startDate = moment('2017-01-01T00:00:00Z').utcOffset(0);
      const dateRange = {end: endDate, start: startDate};
      expect(service.addDateRange(filters, dateRange)).toEqual([
        {'type': 'Profile', 'value': '456'},
        {'end_time': endDate.format('YYYY-MM-DDTHH:mm:ssZ')},
        {'start_time': startDate.format('YYYY-MM-DDTHH:mm:ssZ')}
      ]);
    });
  });

  describe('getFailures()', () => {
    it('fetches failures', () => {
      const types = ['platform', 'environment'];
      const filters = [{type: {name: 'profile'}, value: {id: '456'}}];

      const expectedUrl = `${COMPLIANCE_URL}/reporting/stats/failures`;
      const expectedData = [{'platform': [], 'environment': []}];

      service.getFailures(types, filters).subscribe(failures => {
        expect(failures).toEqual(expectedData);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('POST');

      req.flush(expectedData);
    });
  });

  describe('getNodeTrend()', () => {
    it('fetches trend data for the nodes', () => {
      const endDate = moment('2017-01-31T00:00:00Z').utcOffset(0);
      const startDate = moment('2017-01-01T00:00:00Z').utcOffset(0);
      const filters = [
        {type: {name: 'platform'}, value: {text: 'centos'}},
        {'start_time': startDate},
        {'end_time': endDate}
      ];

      const expectedUrl = `${COMPLIANCE_URL}/reporting/stats/trend`;
      const expectedResponse = [{
        'time': '2017-03-05T00:00:00+0000',
        'compliant': 7,
        'noncompliant': 10
      }];
      const mockResp = {trends: expectedResponse};

      service.getNodeTrend(filters).subscribe(data => {
        expect(data).toEqual(expectedResponse);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('POST');

      req.flush(mockResp);
    });
  });

  describe('getControlsTrend()', () => {
    it('fetches trend data for the controls', () => {
      const endDate = moment('2017-01-31T00:00:00-04:00').utcOffset(-240);
      const startDate = moment('2017-01-01T00:00:00+04:00').utcOffset(240);
      const filters = [
        {type: {name: 'platform'}, value: {text: 'centos'}},
        {'start_time': startDate},
        {'end_time': endDate}
      ];

      const expectedUrl = `${COMPLIANCE_URL}/reporting/stats/trend`;
      const expectedResponse = [{
        'time': '2017-03-05T00:00:00+0000',
        'passed': 3,
        'failed': 10,
        'skipped': 6
      }];
      const mockResp = {trends: expectedResponse};

      service.getControlsTrend(filters).subscribe(data => {
        expect(data).toEqual(expectedResponse);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('POST');

      req.flush(mockResp);
    });
  });

  describe('getSummary()', () => {
    it('fetches profiles data', () => {
      const filters = [{type: {name: 'profile'}, value: {id: '456'}}];

      const expectedUrl = `${COMPLIANCE_URL}/reporting/stats/summary`;
      const expectedData = {};
      const mockResp = {report_summary: expectedData};

      service.getSummary(filters).subscribe(data => {
        expect(data).toEqual(expectedData);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('POST');

      req.flush(mockResp);
    });
  });

  describe('getReports()', () => {
    it('fetches an array of reports', () => {
      const filters = [{type: {name: 'profile'}, value: {id: '456'}}];
      const params = {sort: 'end_time', order: 'ASC'};

      const expectedUrl = `${COMPLIANCE_URL}/reporting/reports`;
      const expectedData = [];
      const mockResp = {reports: expectedData};

      service.getReports(filters, params).subscribe(data => {
        expect(data).toEqual(expectedData);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('POST');

      req.flush(mockResp);
    });
  });

  describe('getProfileResultsSummary()', () => {
    it('fetches a profile', () => {
      const profileID = '987483729';
      const filters = [{type: {name: 'profile_name'}, value: {text: 'ssh-baseline'}}];
      const expectedUrl = `${COMPLIANCE_URL}/reporting/stats/profiles`;
      const expectedData = {};
      const mockResp = {profile_summary: expectedData};

      service.getProfileResultsSummary(profileID, filters).subscribe(data => {
        expect(data).toEqual(expectedData);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('POST');

      req.flush(mockResp);
    });
  });

  describe('getSingleReport()', () => {
    it('fetches a report', () => {
      const reportID = '987483729';
      const filters = [{type: {name: 'profile_name'}, value: {text: 'ssh-baseline'}}];

      const expectedUrl = `${COMPLIANCE_URL}/reporting/reports/id/${reportID}`;
      const expectedData = {profiles: []};

      service.getSingleReport(reportID, filters).subscribe(data => {
        expect(data).toEqual(expectedData);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('POST');

      req.flush(expectedData);
    });
  });

  describe('getProfileResults()', () => {
    it('fetches a profile', () => {
      const profileID = '987483729';
      const filters = [{type: {name: 'profile_name'}, value: {text: 'ssh-baseline'}}];
      const expectedUrl = `${COMPLIANCE_URL}/reporting/stats/profiles`;
      const expectedData = [];
      const mockResp = {control_stats: expectedData};

      service.getProfileResults(profileID, filters).subscribe(data => {
        expect(data).toEqual(expectedData);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('POST');

      req.flush(mockResp);
    });
  });

  describe('downloadReport()', () => {
    it('fetches a report export as text', done => {
      const url = `${COMPLIANCE_URL}/reporting/export`;
      const type = 'csv';
      const filters = [];
      const text = 'report';

      service.downloadReport(type, filters).subscribe(data => {
        expect(data).toEqual(text);
        done();
      });

      const req = httpTestingController.expectOne(url);
      expect(req.request.method).toEqual('POST');
      expect(req.request.responseType).toEqual('text');
      expect(req.request.body).toEqual({type, filters});

      req.flush(text);
    });
  });

  describe('setStartTimeToBegDayEndTime()', () => {
    it('sets start time to beg of day of end time date', () => {
      expect(service.setStartTimeToBegDayEndTime([
        { 'end_time': moment('2017-11-15T23:59:59Z').utcOffset(0) },
        { 'start_time': moment().format('YYYY-MM-DDTHH:mm:ssZ') }
      ])).toEqual([
        { 'end_time': moment('2017-11-15T23:59:59Z').utcOffset(0) },
        { 'start_time': moment('2017-11-15T23:59:59Z').utcOffset(0).startOf('day') }
      ]);
    });
  });
});
