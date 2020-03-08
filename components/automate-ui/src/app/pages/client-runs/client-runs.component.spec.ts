import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { TestBed, fakeAsync } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { StoreModule } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { runtimeChecks, ngrxReducers } from 'app/ngrx.reducers';
import { EntityStatus } from '../../entities/entities';
import { UpdateNodeFilters } from '../../entities/client-runs/client-runs.actions';
import { TelemetryService } from '../../services/telemetry/telemetry.service';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { ClientRunsComponent } from './client-runs.component';

class MockTelemetryService {
  track() { }
}
import {
  ClientRunsRequests
} from '../../entities/client-runs/client-runs.requests';

describe('ClientRunsComponent', () => {
  let fixture, component;
  let router: Router;
  let route: ActivatedRoute;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        ClientRunsComponent,
        MockComponent({ selector: 'app-client-runs-search-bar' }),
        MockComponent({ selector: 'app-client-runs-table', inputs: ['nodeCount'] }),
        MockComponent({ selector: 'app-node-rollup', inputs: ['name', 'count', 'active'] }),
        MockComponent({ selector: 'app-converge-trend-graph' }),
        MockComponent({ selector: 'app-converge-radial-graph', inputs: ['count'] }),
        MockComponent({ selector: 'chef-sidebar-entry '}),
        MockComponent({ selector: 'app-server-org-filter-sidebar'})
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        ClientRunsRequests,
        FeatureFlagsService
      ],
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(ClientRunsComponent);
    component = fixture.componentInstance;
    router = TestBed.inject(Router);
    route = TestBed.inject(ActivatedRoute);
  });

  describe('sets categoryTypes', () => {
    it('ensure types are included', () => {
      const expected = [
        'attribute', 'cookbook', 'chef_tags', 'chef_version', 'environment', 'name', 'platform',
        'policy_group', 'policy_name', 'policy_revision', 'recipe', 'role', 'resource_name',
        'error', 'organization', 'chef_server'];

      const types = component.categoryTypes.map(type => type.type);

      expect(types.length).toBe(expected.length);

      // Each expected type is in the availableFilterTypesNames
      expected.forEach(expectedType =>
        expect(types.indexOf(expectedType)).toBeGreaterThanOrEqual(0));
    });

    it('ensure types text is sorted', () => {
      const typeText = component.categoryTypes.map(type => type.text);
      const expectedTypeText =
        component.categoryTypes.map( type => type.text).sort(
        (type1, type2) => (type1 < type2 ? -1 : 1));

      expect(expectedTypeText).toEqual(typeText, 'Filter Types are not sorted');
    });
  });

  describe('isNotFirstPage', () => {
    it('No page parameter', () => {
      expect(component.isNotFirstPage([])).toBeFalsy();
    });

    it('has page parameter', () => {
      expect(component.isNotFirstPage([{type: 'page', text: '2'}])).toBeTruthy();
    });
  });

  describe('isCurrentPageEmpty', () => {
    const node =     {
        id: '56343c09-e968-43b3-b896-edd7cca03fbd',
        name: 'A-non-architecto',
        fqdn: 'A-non-architecto.bergnaum.co',
        checkin: null,
        uptime_seconds: 17112079,
        environment: 'test',
        platform: 'solaris',
        policy_group: '',
        organization: '',
        source_fqdn: '',
        status: 'failure',
        latestRunId: '22efcb97-ae0d-4ece-b284-51bc73b2c6cc',
        hasRuns: true,
        lastCcrReceived: new Date(),
        deprecationsCount: 0,
        chefVersion: '12.6.0'
      };

    it('no nodes, loaded successfully', () => {
      expect(component.isCurrentPageEmpty([], EntityStatus.loadingSuccess)).toBeTruthy();
    });

    it('no nodes, load failure', () => {
      expect(component.isCurrentPageEmpty([], EntityStatus.loadingFailure)).toBeFalsy();
    });

    it('no nodes, loading', () => {
      expect(component.isCurrentPageEmpty([], EntityStatus.loading)).toBeFalsy();
    });

    it('no nodes, not loaded', () => {
      expect(component.isCurrentPageEmpty([], EntityStatus.notLoaded)).toBeFalsy();
    });

    it('some nodes, loaded successfully', () => {

      expect(component.isCurrentPageEmpty([node], EntityStatus.loadingSuccess)).toBeFalsy();
    });

    it('some nodes, load failure', () => {
      expect(component.isCurrentPageEmpty([node], EntityStatus.loadingFailure)).toBeFalsy();
    });

    it('some nodes, loading', () => {
      expect(component.isCurrentPageEmpty([node], EntityStatus.loading)).toBeFalsy();
    });

    it('some nodes, not loaded', () => {
      expect(component.isCurrentPageEmpty([node], EntityStatus.notLoaded)).toBeFalsy();
    });
  });

  describe('statusFilter', () => {
    beforeEach(() => spyOn(router, 'navigate'));

    it('when set to an allowed value', () => {
      route.snapshot.queryParams = {};

      component.statusFilter('success');

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: { status: ['success'] }});
    });

    it('when set a not allowed value, the URL is not changed', () => {
      route.snapshot.queryParams = {};

      component.statusFilter('not-allowed');

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: {}});
    });

    it('when set to total', () => {
      route.snapshot.queryParams = {};

      component.statusFilter('total');

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: {}});
    });

    it('when a page is selected and the status is changed the selected page is removed', () => {
      route.snapshot.queryParams = { page: [2], status: ['success'] };

      component.statusFilter('total');

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: {}});
    });

    it('when selection a different status the existing one should be replaced', () => {
      route.snapshot.queryParams = { status: ['success'] };

      component.statusFilter('failure');

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: { status: ['failure'] }});
    });
  });

  describe('onUpdateSort', () => {
    beforeEach(() => spyOn(router, 'navigate'));

    it('when name is selected with ASC', () => {
      route.snapshot.queryParams = {};

      component.onUpdateSort({field: 'name', fieldDirection: 'ASC'});

      expect(router.navigate).toHaveBeenCalledWith(
        [], { queryParams: { sortField: ['name'], sortDirection: ['ASC'] }});
    });

    it('when sort is selected an incorrect type, the sort is not changed', () => {
      route.snapshot.queryParams = {};

      component.onUpdateSort({field: 'wrong', fieldDirection: 'ASC'});

      expect(router.navigate).not.toHaveBeenCalled();
    });

    it('when a page is selected and the sort is changed the selected page is removed', () => {
      route.snapshot.queryParams = { page: [2] };

      component.onUpdateSort({field: 'name', fieldDirection: 'ASC'});

      expect(router.navigate).toHaveBeenCalledWith(
        [], { queryParams: { sortField: ['name'], sortDirection: ['ASC'] }});
    });

    it('when sort is selected an incorrect sort direction, the sort is not changed', () => {
      route.snapshot.queryParams = {};

      component.onUpdateSort({field: 'name', fieldDirection: 'wrong'});

      expect(router.navigate).not.toHaveBeenCalled();
    });
  });

  describe('onFiltersClear', () => {
    beforeEach(() => spyOn(router, 'navigate'));

    it('when page is selected remove page selection', () => {
      route.snapshot.queryParams = { page: [2], name: ['chef-*'] };

      component.onFiltersClear({});

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: {}});
    });

    it('do not remove status or sort selections', () => {
      route.snapshot.queryParams = {
        status: ['success'],
        sortField: ['name'],
        sortDirection: ['ASC'],
        name: ['chef-*']
      };

      component.onFiltersClear({});

      expect(router.navigate).toHaveBeenCalledWith(
        [], { queryParams: { status: ['success'], sortField: ['name'], sortDirection: ['ASC'] }});
    });

    it('all search bar filters are removed', () => {
      route.snapshot.queryParams = {
        attribute: ['attribute'],
        cookbook: ['apple_pie'],
        environment: ['environment'],
        name: ['name'],
        platform: ['platform'],
        policy_group: ['policy_group'],
        policy_name: ['policy_name'],
        policy_revision: ['policy_revision'],
        recipe: ['recipe'],
        resource_name: ['resource_name'],
        role: ['role']
      };

      component.onFiltersClear({});

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: {}});
    });
  });

  describe('onPageChange', () => {
    beforeEach(() => spyOn(router, 'navigate'));

    it('when the first page is selected remove page from URL', () => {
      route.snapshot.queryParams = {};

      component.onPageChange(1);

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: {}});
    });

    it('when a page is selected', () => {
      route.snapshot.queryParams = {};

      component.onPageChange(2);

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: { page: [2] }});
    });

    it('when a page is a negative number do not change the URL', () => {
      route.snapshot.queryParams = {};

      component.onPageChange(-2);

      expect(router.navigate).not.toHaveBeenCalled();
    });

    it('when a page is 0 number do not change the URL', () => {
      route.snapshot.queryParams = {};

      component.onPageChange(0);

      expect(router.navigate).not.toHaveBeenCalled();
    });

    it('when a page changes do not remove existing URL parameters', () => {
      route.snapshot.queryParams = {
        attribute: ['attribute'],
        cookbook: ['apple_pie'],
        environment: ['environment'],
        name: ['name']
      };

      component.onPageChange(2);

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: {
        attribute: ['attribute'],
        cookbook: ['apple_pie'],
        environment: ['environment'],
        name: ['name'],
        page: [2]
      }});
    });
  });

  describe('onFilterRemoved', () => {
    beforeEach(() => spyOn(router, 'navigate'));

    it('removing only one search filter', () => {
      route.snapshot.queryParams = {
        attribute: ['attribute'],
        cookbook: ['apple_pie'],
        environment: ['environment'],
        name: ['name']
      };

      component.onFilterRemoved({detail: {type: 'name', text: 'name'}});

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: {
        attribute: ['attribute'],
        cookbook: ['apple_pie'],
        environment: ['environment']
      }});
    });

    it('removing one filter when there are multiple filters of the same type', () => {
      route.snapshot.queryParams = {
        attribute: ['attribute'],
        cookbook: ['apple_pie'],
        environment: ['environment'],
        name: ['name', 'chef-*']
      };

      component.onFilterRemoved({detail: {type: 'name', text: 'name'}});

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: {
        attribute: ['attribute'],
        cookbook: ['apple_pie'],
        environment: ['environment'],
        name: ['chef-*']
      }});
    });

    it('removing a filter where the value is not there, should not change the URL', () => {
      route.snapshot.queryParams = {
        attribute: ['attribute'],
        cookbook: ['apple_pie'],
        environment: ['environment'],
        name: ['name']
      };

      component.onFilterRemoved({detail: {type: 'name', text: 'bob'}});

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: {
        attribute: ['attribute'],
        cookbook: ['apple_pie'],
        environment: ['environment'],
        name: ['name']
      }});
    });

    it('removing a filter where the type is not there, should not change the URL', () => {
      route.snapshot.queryParams = {
        attribute: ['attribute'],
        cookbook: ['apple_pie'],
        environment: ['environment']
      };

      component.onFilterRemoved({detail: {type: 'name', text: 'bob'}});

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: {
        attribute: ['attribute'],
        cookbook: ['apple_pie'],
        environment: ['environment']
      }});
    });
  });

  describe('onFilterAdded', () => {
    beforeEach(() => spyOn(router, 'navigate'));

    it('when adding a filter', () => {
      route.snapshot.queryParams = {};

      component.onFilterAdded({detail: {type: 'name', text: 'name'}});

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: { name: ['name'] }});
    });

    it('when adding a filter to a type that already has a filter', () => {
      route.snapshot.queryParams = { name: ['name'] };

      component.onFilterAdded({detail: {type: 'name', text: 'tim'}});

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: { name: ['name', 'tim'] }});
    });

    it('when adding a filter when there is already a different filter type', () => {
      route.snapshot.queryParams = { name: ['name'] };

      component.onFilterAdded({detail: {type: 'cookbook', text: 'apple_pie'}});

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: {
        name: ['name'],
        cookbook: ['apple_pie']
      }});
    });

    it('when adding an incorrect type filter, the URL should not change', () => {
      route.snapshot.queryParams = { name: ['name'] };

      component.onFilterAdded({detail: {type: 'wrong', text: 'apple_pie'}});

      expect(router.navigate).not.toHaveBeenCalled();
    });

    it('when filter is updated the selected page is removed', () => {
      route.snapshot.queryParams = { page: [2] };

      component.onFilterAdded({detail: {type: 'name', text: 'tim'}});

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: { name: ['tim'] }});
    });

    it('when a incorrect filter type is updated, the URL should not change', () => {
      route.snapshot.queryParams = { page: [2] };

      component.onFilterAdded({detail: {type: 'wrong', text: 'tim'}});

      expect(router.navigate).not.toHaveBeenCalled();
    });
  });

  describe('updateNodeFilters', () => {
    it('when the URL is empty', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateNodeFilters([]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateNodeFilters({filters: {
          page: 1,
          pageSize: 100,
          searchBar: [],
          sortField: 'name',
          sortDirection: 'ASC',
          status: undefined
        }}));
    }));

    it('when the URL has search bar filters', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateNodeFilters([{type: 'name', text: 'bob'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateNodeFilters({filters: {
          page: 1,
          pageSize: 100,
          searchBar: [{type: 'name', text: 'bob'}],
          sortField: 'name',
          sortDirection: 'ASC',
          status: undefined
        }}));
    }));

    it('when the URL has a parameter not involved, '
      + 'it should not be used in the filter', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateNodeFilters([{type: 'not-used', text: 'bob'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateNodeFilters({filters: {
          page: 1,
          pageSize: 100,
          searchBar: [],
          sortField: 'name',
          sortDirection: 'ASC',
          status: undefined
        }}));
    }));

    it('when the URL has a page selected', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateNodeFilters([{type: 'page', text: '2'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateNodeFilters({filters: {
          page: 2,
          pageSize: 100,
          searchBar: [],
          sortField: 'name',
          sortDirection: 'ASC',
          status: undefined
        }}));
    }));

    it('when the URL has a page selected with an invalid value', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateNodeFilters([{type: 'page', text: 'wrong'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateNodeFilters({filters: {
          page: 1,
          pageSize: 100,
          searchBar: [],
          sortField: 'name',
          sortDirection: 'ASC',
          status: undefined
        }}));
    }));

    it('when the URL has a page selected with an invalid number', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateNodeFilters([{type: 'page', text: '-1'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateNodeFilters({filters: {
          page: 1,
          pageSize: 100,
          searchBar: [],
          sortField: 'name',
          sortDirection: 'ASC',
          status: undefined
        }}));
    }));

    it('when the URL has a sort selected', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateNodeFilters([{type: 'sortField', text: 'uptime_seconds'},
        {type: 'sortDirection', text: 'DESC'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateNodeFilters({filters: {
          page: 1,
          pageSize: 100,
          searchBar: [],
          sortField: 'uptime_seconds',
          sortDirection: 'DESC',
          status: undefined
        }}));
    }));

    it('when the URL has a status selected', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateNodeFilters([{type: 'status', text: 'success'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateNodeFilters({filters: {
          page: 1,
          pageSize: 100,
          searchBar: [],
          sortField: 'name',
          sortDirection: 'ASC',
          status: 'success'
        }}));
    }));

    it('when the URL has an invalid status selected', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateNodeFilters([{type: 'status', text: 'wrong'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateNodeFilters({filters: {
          page: 1,
          pageSize: 100,
          searchBar: [],
          sortField: 'name',
          sortDirection: 'ASC',
          status: undefined
        }}));
    }));
  });
});
