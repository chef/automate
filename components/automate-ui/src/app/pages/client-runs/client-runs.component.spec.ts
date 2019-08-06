import { TestBed, fakeAsync, tick } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { MockComponent } from 'ng2-mock-component';

import { ClientRunsComponent } from './client-runs.component';
import { StoreModule } from '@ngrx/store';
import * as sidebar from '../../services/sidebar/sidebar.reducer';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { Router } from '@angular/router';
import { UpdateNodeFilters } from '../../entities/client-runs/client-runs.actions';
import { TelemetryService } from '../../services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}
import {
  ClientRunsRequests
} from '../../entities/client-runs/client-runs.requests';

describe('ClientRunsComponent', () => {
  let fixture, component;
  let router: Router;

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
        ClientRunsRequests
      ],
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        StoreModule.forRoot({
          sidebar: sidebar.sidebarReducer
        })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(ClientRunsComponent);
    component = fixture.componentInstance;
    router = TestBed.get(Router);
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

  describe('statusFilter', () => {
    it('when set to an allowed value', () => {
      spyOn(component.router, 'navigate');
      component.statusFilter('success');
      expect(component.router.navigate).toHaveBeenCalledWith(
        [], {queryParams: { status: ['success'] }});
    });

    it('when set a not allowed value, the URL is not changed', () => {
      spyOn(component.router, 'navigate');
      component.statusFilter('not-allowed');
      expect(component.router.navigate).toHaveBeenCalledWith(
        [], {queryParams: { }});
    });

    it('when set to total', () => {
      spyOn(component.router, 'navigate');
      component.statusFilter('total');
      expect(component.router.navigate).toHaveBeenCalledWith(
        [], {queryParams: { }});
    });

    it('when a page is selected and the status is changed ' +
    'the selected page is removed', fakeAsync(() => {
      router.navigate([''], {queryParams: { page: [2], status: ['success'] }});

      tick();

      component.statusFilter('total');

      tick();

      expect('/').toEqual(router.routerState.snapshot.url);
    }));

    it('when selection a different status the existing one should be replaced', fakeAsync(() => {
      router.navigate([''], {queryParams: { status: ['success'] }});

      tick();

      component.statusFilter('failure');

      tick();

      expect('/?status=failure').toEqual(router.routerState.snapshot.url);
    }));
  });

  describe('onUpdateSort', () => {
    it('when name is selected with ASC', fakeAsync(() => {
      router.navigate([''], {queryParams: { }});

      tick();

      component.onUpdateSort({field: 'name', fieldDirection: 'ASC'});

      tick();

      expect('/?sortField=name&sortDirection=ASC').toEqual(router.routerState.snapshot.url);
    }));

    it('when sort is selected an incorrect type, the sort is not changed', fakeAsync(() => {
      router.navigate([''], {queryParams: { }});

      tick();

      component.onUpdateSort({field: 'wrong', fieldDirection: 'ASC'});

      tick();

      expect('/').toEqual(router.routerState.snapshot.url);
    }));

    it('when a page is selected and the sort is changed ' +
    'the selected page is removed', fakeAsync(() => {
      router.navigate([''], {queryParams: { page: [2] }});

      tick();

      component.onUpdateSort({field: 'name', fieldDirection: 'ASC'});

      tick();

      expect('/?sortField=name&sortDirection=ASC').toEqual(router.routerState.snapshot.url);
    }));

    it('when sort is selected an incorrect sort direction, ' +
      'the sort is not change', fakeAsync(() => {
      router.navigate([''], {queryParams: { }});

      tick();

      component.onUpdateSort({field: 'name', fieldDirection: 'wrong'});

      tick();

      expect('/').toEqual(router.routerState.snapshot.url);
    }));
  });

  describe('onFiltersClear', () => {
    it('when page is selected remove page selection', fakeAsync(() => {
      router.navigate([''], {queryParams: { page: [2], name: ['chef-*'] }});

      tick();

      component.onFiltersClear({});

      tick();

      expect('/').toEqual(router.routerState.snapshot.url);
    }));

    it('do not remove status or sort selections', fakeAsync(() => {
      router.navigate([''], {queryParams: { status: ['success'], sortField: ['name'],
        sortDirection: ['ASC'], name: ['chef-*'] }});

      tick();

      component.onFiltersClear({});

      tick();

      expect('/?status=success&sortField=name&sortDirection=ASC').
        toEqual(router.routerState.snapshot.url);
    }));

    it('all search bar filters are removed', fakeAsync(() => {
      router.navigate([''], {queryParams: {
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
        role: ['role']}});

      tick();

      component.onFiltersClear({});

      tick();

      expect('/').toEqual(router.routerState.snapshot.url);
    }));
  });

  describe('onPageChange', () => {
    it('when the first page is selected remove page from URL', fakeAsync(() => {
      router.navigate([''], {queryParams: { }});

      tick();

      component.onPageChange(1);

      tick();

      expect('/').toEqual(router.routerState.snapshot.url);
    }));

    it('when a page is selected', fakeAsync(() => {
      router.navigate([''], {queryParams: { }});

      tick();

      component.onPageChange(2);

      tick();

      expect('/?page=2').toEqual(router.routerState.snapshot.url);
    }));

    it('when a page is a negative number do not change the URL', fakeAsync(() => {
      router.navigate([''], {queryParams: { }});

      tick();

      component.onPageChange(-2);

      tick();

      expect('/').toEqual(router.routerState.snapshot.url);
    }));

    it('when a page is 0 number do not change the URL', fakeAsync(() => {
      router.navigate([''], {queryParams: { }});

      tick();

      component.onPageChange(0);

      tick();

      expect('/').toEqual(router.routerState.snapshot.url);
    }));

    it('when a page changes do not remove existing URL parameters', fakeAsync(() => {
      router.navigate([''], {queryParams: { attribute: ['attribute'], cookbook: ['apple_pie'],
        environment: ['environment'], name: ['name']}});

      tick();

      component.onPageChange(2);

      tick();

      expect('/?attribute=attribute&cookbook=apple_pie&environment=environment&name=name&page=2').
        toEqual(router.routerState.snapshot.url);
    }));
  });

  describe('onFilterRemoved', () => {
    it('removing only one search filter', fakeAsync(() => {
      router.navigate([''], {queryParams: {
        attribute: ['attribute'],
        cookbook: ['apple_pie'],
        environment: ['environment'],
        name: ['name']}});

      tick();

      component.onFilterRemoved({detail: {type: 'name', text: 'name'}});

      tick();

      expect('/?attribute=attribute&cookbook=apple_pie&environment=environment')
        .toEqual(router.routerState.snapshot.url);
    }));

    it('removing one filter when there are multiple filters of the same type', fakeAsync(() => {
      router.navigate([''], {queryParams: {
        attribute: ['attribute'],
        cookbook: ['apple_pie'],
        environment: ['environment'],
        name: ['name', 'chef-*']}});

      tick();

      component.onFilterRemoved({detail: {type: 'name', text: 'name'}});

      tick();

      expect('/?attribute=attribute&cookbook=apple_pie&environment=environment&name=chef-*')
        .toEqual(router.routerState.snapshot.url);
    }));

    it('removing a filter where the value is not there,' +
      ' should not change the URL', fakeAsync(() => {
      router.navigate([''], {queryParams: {
        attribute: ['attribute'],
        cookbook: ['apple_pie'],
        environment: ['environment'],
        name: ['name']}});

      tick();

      component.onFilterRemoved({detail: {type: 'name', text: 'bob'}});

      tick();

      expect('/?attribute=attribute&cookbook=apple_pie&environment=environment&name=name')
        .toEqual(router.routerState.snapshot.url);
    }));

    it('removing a filter where the type is not there,' +
    ' should not change the URL', fakeAsync(() => {
      router.navigate([''], {queryParams: {
        attribute: ['attribute'],
        cookbook: ['apple_pie'],
        environment: ['environment']}});

      tick();

      component.onFilterRemoved({detail: {type: 'name', text: 'bob'}});

      tick();

      expect('/?attribute=attribute&cookbook=apple_pie&environment=environment')
        .toEqual(router.routerState.snapshot.url);
    }));
  });

  describe('onFilterAdded', () => {
    it('when adding a filter', fakeAsync(() => {
      router.navigate([''], {queryParams: {}});

      tick();

      component.onFilterAdded({detail: {type: 'name', text: 'name'}});

      tick();

      expect('/?name=name').toEqual(router.routerState.snapshot.url);
    }));

    it('when adding a filter to a type that already has a filter', fakeAsync(() => {
      router.navigate([''], {queryParams: {name: ['name']}});

      tick();

      component.onFilterAdded({detail: {type: 'name', text: 'tim'}});

      tick();

      expect('/?name=name&name=tim').toEqual(router.routerState.snapshot.url);
    }));

    it('when adding a filter when there is already a different filter type', fakeAsync(() => {
      router.navigate([''], {queryParams: {name: ['name']}});

      tick();

      component.onFilterAdded({detail: {type: 'cookbook', text: 'apple_pie'}});

      tick();

      expect('/?name=name&cookbook=apple_pie').toEqual(router.routerState.snapshot.url);
    }));

    it('when adding an incorrect type filter, the URL should not change', fakeAsync(() => {
      router.navigate([''], {queryParams: {name: ['name']}});

      tick();

      component.onFilterAdded({detail: {type: 'wrong', text: 'apple_pie'}});

      tick();

      expect('/?name=name').toEqual(router.routerState.snapshot.url);
    }));

    it('when filter is updated the selected page is removed', fakeAsync(() => {
      router.navigate([''], {queryParams: {page: [2]}});

      tick();

      component.onFilterAdded({detail: {type: 'name', text: 'tim'}});

      tick();

      expect('/?name=tim').toEqual(router.routerState.snapshot.url);
    }));

    it('when a incorrect filter type is updated,' +
      ' the URL should not change', fakeAsync(() => {
      router.navigate([''], {queryParams: {page: [2]}});

      tick();

      component.onFilterAdded({detail: {type: 'wrong', text: 'tim'}});

      tick();

      expect('/?page=2').toEqual(router.routerState.snapshot.url);
    }));
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
