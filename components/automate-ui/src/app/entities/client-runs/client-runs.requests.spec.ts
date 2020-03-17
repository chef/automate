import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';

import { ClientRunsRequests } from './client-runs.requests';
import { NodeFilter, NodeCount, Chicklet } from '../../types/types';

import { HttpParams } from '@angular/common/http';

import { environment } from '../../../environments/environment';
const CONFIG_MGMT_URL = environment.config_mgmt_url;

describe('ClientRunsRequests', () => {
  let httpTestingController: HttpTestingController;
  let service: ClientRunsRequests;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule
      ],
      providers: [
        ClientRunsRequests
      ]
    });

    service = TestBed.inject(ClientRunsRequests);
    httpTestingController = TestBed.inject(HttpTestingController);
  });

  describe('getSuggestions()', () => {
    it('returns all provided values', () => {
      const expectedData: Chicklet[] = [];
      for (let i = 0; i < 50; i++) {
        expectedData.push({'text': i.toString(), 'type': 'name'});
      }
      const expectedUrl = `${CONFIG_MGMT_URL}/suggestions?type=name&text=fred`;
      const filters: NodeFilter = <NodeFilter>{};

      service.getSuggestions('name', 'fred', filters).subscribe(data => {
        expect(data.length).toEqual(50);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('GET');

      req.flush(expectedData);
    });

    it('returns 10 values when 10 are provided', () => {
      const expectedData: Chicklet[] = [];
      for (let i = 0; i < 10; i++) {
        expectedData.push({'text': i.toString(), 'type': 'name'});
      }
      const expectedUrl = `${CONFIG_MGMT_URL}/suggestions?type=name&text=fred`;

      const filters: NodeFilter = <NodeFilter>{};
      service.getSuggestions('name', 'fred', filters).subscribe(data => {
        expect(data.length).toEqual(10);
        for ( const item of data ) {
          expect(Number(item.text)).toBeLessThan(10);
        }
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('GET');

      req.flush(expectedData);
    });

    it('remove empty suggestions', () => {
      const expectedData: any[] = [];
      for (let i = 0; i < 10; i++) {
        expectedData.push({'text': i.toString(), 'type': 'name'});
      }

      // Add empty suggestions
      expectedData.push({'type': ''});
      expectedData.push({'text': '', 'type': 'name'});

      const expectedUrl = `${CONFIG_MGMT_URL}/suggestions?type=name&text=fred`;

      const filters: NodeFilter = <NodeFilter>{};
      service.getSuggestions('name', 'fred', filters).subscribe(data => {
        expect(data.length).toEqual(10);
        for ( const item of data ) {
          expect(Number(item.text)).toBeLessThan(10);
        }
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('GET');

      req.flush(expectedData);
    });

    it('narrowing suggestions', () => {
      const expectedData: Chicklet[] = [];
      for (let i = 0; i < 10; i++) {
        expectedData.push({'text': i.toString(), 'type': ''});
      }
      const expectedUrl = CONFIG_MGMT_URL + '/suggestions?' +
      'filter=name:bob&filter=chef_server:chef.org&filter=organization:org1&type=name&text=fred';

      const filters: NodeFilter = <NodeFilter>{
        page: 0,
        pageSize: 100,
        status: 'success',
        searchBar: [
          {'text': 'bob', 'type': 'name'},
          {'text': 'chef.org', 'type': 'chef_server'},
          {'text': 'org1', 'type': 'organization'}]
      };

      service.getSuggestions('name', 'fred', filters).subscribe();

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('GET');
    });
  });

  describe('getNodeCount()', () => {
    it('fetches node count data', () => {
      const expectedUrl = `${CONFIG_MGMT_URL}/stats/node_counts`;
      const expectedData: NodeCount = {total: 0, success: 0, failure: 0, missing: 0};

      service.getNodeCount().subscribe(data => {
        expect(data).toEqual(expectedData);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('GET');

      req.flush(expectedData);
    });
  });

  describe('filtering by environment', () => {
    it('encodes the value only once', () => {
      const filters: NodeFilter = <NodeFilter>{
        page: 0,
        pageSize: 100,
        sortField: 'name',
        sortDirection: 'asc',
        searchBar: [{type: 'environment', text: 'Dev Sec Beta'}]
      };

      const expectedPath = `${CONFIG_MGMT_URL}/stats/node_counts`;
      const expectedSearch = 'filter=environment:Dev%2520Sec%2520Beta';
      const expectedUrl = `${expectedPath}?${expectedSearch}`;
      const expectedData: NodeCount = {total: 0, success: 0, failure: 0, missing: 0};

      service.getNodeCount(filters).subscribe(data => {
        expect(data).toEqual(expectedData);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('GET');

      req.flush(expectedData);
    });
  });

  describe('filtering by node name', () => {
    it('encodes the value only once', () => {
      const filters: NodeFilter = <NodeFilter>{
        page: 0,
        pageSize: 100,
        sortField: 'name',
        sortDirection: 'asc',
        searchBar: [{type: 'name', text: 'HappyNode'}]
      };

      const expectedPath = `${CONFIG_MGMT_URL}/stats/node_counts`;
      const expectedSearch = 'filter=name:HappyNode';
      const expectedUrl = `${expectedPath}?${expectedSearch}`;
      const expectedData: NodeCount = {total: 0, success: 0, failure: 0, missing: 0};

      service.getNodeCount(filters).subscribe(data => {
        expect(data).toEqual(expectedData);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('GET');

      req.flush(expectedData);
    });
  });

  describe('filtering by platform and environment', () => {
    it('encodes the value only once', () => {
      const filters: NodeFilter = <NodeFilter>{
        page: 0,
        pageSize: 100,
        sortField: 'name',
        sortDirection: 'asc',
        searchBar: [{type: 'environment', text: 'Dev Sec'},
                    {type: 'platform', text: 'Bla platform'}]
      };

      const expectedPath = `${CONFIG_MGMT_URL}/stats/node_counts`;
      const expectedSearch = 'filter=environment:Dev%2520Sec&filter=platform:Bla%2520platform';
      const expectedUrl = `${expectedPath}?${expectedSearch}`;
      const expectedData: NodeCount = {total: 0, success: 0, failure: 0, missing: 0};

      service.getNodeCount(filters).subscribe(data => {
        expect(data).toEqual(expectedData);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('GET');

      req.flush(expectedData);
    });
  });

  describe('filtering by platform , org and server', () => {
    it('encodes the value only once', () => {
      const filters: NodeFilter = <NodeFilter>{
        page: 0,
        pageSize: 100,
        sortField: 'name',
        sortDirection: 'asc',
        searchBar: [{type: 'platform', text: 'Bla bla platform'},
         {type: 'chef_server', text: 'chefserver1'}, {type: 'organization', text: 'megaOrg'}]
      };

      const expectedPath = `${CONFIG_MGMT_URL}/stats/node_counts`;
      const expectedSearch = 'filter=platform:Bla%2520bla%2520platform' +
        '&filter=chef_server:chefserver1' +
        '&filter=organization:megaOrg';
      const expectedUrl = `${expectedPath}?${expectedSearch}`;
      const expectedData: NodeCount = {total: 0, success: 0, failure: 0, missing: 0};

      service.getNodeCount(filters).subscribe(data => {
        expect(data).toEqual(expectedData);
      });

      const req = httpTestingController.expectOne(expectedUrl);

      expect(req.request.method).toEqual('GET');

      req.flush(expectedData);
    });
  });

  describe('build URLSearchParams', () => {
    it('empty filters empty url params', () => {
      const result: HttpParams = service.buildURLSearchParams({});

      expect(result.keys.length).toEqual(0);
    });

    it('multiple servers in filter multiple params', () => {
      const result: HttpParams = service.buildURLSearchParams({ searchBar:
        [{type: 'chef_server', text: 'fake.com'}, {type: 'chef_server', text: 'example.com'}]});

      expect(2).toEqual(result.getAll('filter').length);
      const elements = result.getAll('filter');
      expect(elements.indexOf('chef_server:fake.com')).toBeGreaterThan(-1);
      expect(elements.indexOf('chef_server:example.com')).toBeGreaterThan(-1);
    });

    it('multiple organizations in filter multiple params', () => {
      const result: HttpParams = service.buildURLSearchParams({ searchBar:
        [{type: 'organization', text: 'Golds'}, {type: 'organization', text: 'CapitalOne'}]});
      const elements = result.getAll('filter');

      expect(2).toEqual(result.getAll('filter').length);
      expect(elements.indexOf('organization:Golds')).toBeGreaterThan(-1);
      expect(elements.indexOf('organization:CapitalOne')).toBeGreaterThan(-1);
    });

    it('all search pills in filter all search pills in params', () => {
      const pills = [
        {type: 'name', text: 'name1'},
        {type: 'platform', text: 'platform1'},
        {type: 'environment', text: 'environment1'},
        {type: 'role', text: 'role1'},
        {type: 'cookbook', text: 'cookbook1'},
        {type: 'recipe', text: 'recipe1'},
        {type: 'resource_name', text: 'resource_name1'},
        {type: 'attribute', text: 'attribute1'},
        {type: 'policy_group', text: 'policy_group1'},
        {type: 'policy_name', text: 'policy_name1'},
        {type: 'policy_revision', text: 'policy_revision1'}
      ];
      const result: HttpParams = service.buildURLSearchParams({searchBar: pills});
      const elements = result.getAll('filter');

      expect(11).toEqual(result.getAll('filter').length);
      expect(elements.indexOf('name:name1')).toBeGreaterThan(-1);
      expect(elements.indexOf('platform:platform1')).toBeGreaterThan(-1);
      expect(elements.indexOf('environment:environment1')).toBeGreaterThan(-1);
      expect(elements.indexOf('role:role1')).toBeGreaterThan(-1);
      expect(elements.indexOf('cookbook:cookbook1')).toBeGreaterThan(-1);
      expect(elements.indexOf('recipe:recipe1')).toBeGreaterThan(-1);
      expect(elements.indexOf('resource_name:resource_name1')).toBeGreaterThan(-1);
      expect(elements.indexOf('attribute:attribute1')).toBeGreaterThan(-1);
      expect(elements.indexOf('policy_group:policy_group1')).toBeGreaterThan(-1);
      expect(elements.indexOf('policy_name:policy_name1')).toBeGreaterThan(-1);
      expect(elements.indexOf('policy_revision:policy_revision1')).toBeGreaterThan(-1);
    });

    it('duplicate types of pills in filter, both items in params', () => {
      const pills = [
        {type: 'name', text: 'name1'},
        {type: 'name', text: 'name2'}
      ];
      const result: HttpParams = service.buildURLSearchParams({searchBar: pills});
      const nameElements = result.getAll('filter');

      expect(nameElements.indexOf('name:name1')).toBeGreaterThan(-1);
      expect(nameElements.indexOf('name:name2')).toBeGreaterThan(-1);
      expect(2).toEqual(result.getAll('filter').length);
    });
  });
});
