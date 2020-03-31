import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { TestBed, fakeAsync, ComponentFixture } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { StoreModule, Store } from '@ngrx/store';

import { NgrxStateAtom, runtimeChecks } from 'app/ngrx.reducers';
import { ServiceStatusIconPipe } from 'app/pipes/service-status-icon.pipe';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { serviceGroupsEntityReducer } from 'app/entities/service-groups/service-groups.reducer';
import {
  UpdateServiceGroupsFilters,
  GetServiceGroupsCountsSuccess
} from 'app/entities/service-groups/service-groups.actions';
import { ServiceGroupsComponent  } from './service-groups.component';
import { ServiceGroupsRequests } from 'app/entities/service-groups/service-groups.requests';
import { Observable, of as observableOf } from 'rxjs';
class MockTelemetryService {
  track(_event?: string, _properties?: any): void {

  }
}

class MockServiceGroupsRequests {
  getServiceStats(): Observable<Object> {
    return observableOf({
      totalServiceGroups: 0,
      totalServices: 0,
      totalSupervisors: 0,
      totalDeployments: 0
    });
  }
}

describe('ServiceGroupsComponent', () => {
  let fixture: ComponentFixture<ServiceGroupsComponent>;
  let component: ServiceGroupsComponent;
  let router: Router;
  let route: ActivatedRoute;
  let ngrxStore: Store<NgrxStateAtom>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        ServiceStatusIconPipe,
        ServiceGroupsComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        { provide: ServiceGroupsRequests, useClass: MockServiceGroupsRequests }],
      imports: [
        StoreModule.forRoot({
          serviceGroups: serviceGroupsEntityReducer
        }, { runtimeChecks }),
        RouterTestingModule
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(ServiceGroupsComponent);
    component = fixture.componentInstance;
    router = TestBed.inject(Router);
    route = TestBed.inject(ActivatedRoute);
    ngrxStore = TestBed.inject(Store);
    component.ngOnInit();
  });

  describe('when the page first loads', () => {
    describe('with defaults', () => {
      it('should return 0 for the total number of service groups', fakeAsync(() => {
        expect(component.totalServiceGroups).toEqual(0);
      }));
    });

    describe('with ServiceGroupsCounts', () => {
      beforeEach(() => {
        ngrxStore.dispatch(new GetServiceGroupsCountsSuccess({
          total: 21,
          ok: 10,
          warning: 5,
          critical: 5,
          unknown: 1,
          disconnected: 0
        }));
      });

      it('should update the total number of service groups', fakeAsync(() => {
        expect(component.totalServiceGroups).toEqual(21);
      }));

      describe('and OK status filter update', () => {
        beforeEach(() => {
          ngrxStore.dispatch(new UpdateServiceGroupsFilters({filters: {status: 'ok'}}));
        });

        it('should update the total number of service groups and selected status', fakeAsync(() => {
          expect(component.selectedStatus ).toEqual('ok');
          expect(component.totalServiceGroups).toEqual(10);
        }));
      });
    });
  });

  describe('statusFilter', () => {
    beforeEach(() => spyOn(router, 'navigate'));

    it('when set to an allowed value', () => {
      route.snapshot.queryParams = {};

      component.statusFilter('critical');

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: { status: ['critical'] }});
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

    it('when selection a different status the existing one should be replaced', () => {
      route.snapshot.queryParams['status'] = ['ok'];

      component.statusFilter('unknown');

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: { status: ['unknown'] }});
    });
  });

  describe('onToggleSort', () => {
    beforeEach(() => spyOn(router, 'navigate'));

    describe('with defaults', () => {
      it('should return "name" as sorting field', () => {
        expect(component.defaultSortField ).toEqual('percent_ok');
        expect(component.currentSortField ).toEqual('percent_ok');
        expect(component.currentFieldDirection).toEqual('ASC');
      });

      it('should change sort direction on already selected sorting field', () => {
        route.snapshot.queryParams = {};

        component.onToggleSort('percent_ok');

        expect(router.navigate).toHaveBeenCalledWith(
          [], { queryParams: { sortField: ['percent_ok'], sortDirection: ['DESC'] }});
      });
    });

    it('when set to an allowed value', () => {
      route.snapshot.queryParams = {};

      component.onToggleSort('environment');

      expect(router.navigate).toHaveBeenCalledWith(
        [], { queryParams: { sortField: ['environment'], sortDirection: ['ASC'] }});
    });

    it('when set to an allowed value', () => {
      route.snapshot.queryParams = {};

      component.onToggleSort('name');

      expect(router.navigate).toHaveBeenCalledWith(
        [], { queryParams: { sortField: ['name'], sortDirection: ['ASC'] }});
    });

    it('when set to an allowed value', () => {
      route.snapshot.queryParams = {};

      component.onToggleSort('app_name');

      expect(router.navigate).toHaveBeenCalledWith(
        [], { queryParams: { sortField: ['app_name'], sortDirection: ['ASC'] }});
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

      expect(router.navigate).toHaveBeenCalledWith([], { queryParams: { page: 2 }});
    });

    it('when a page is a negative number do not change the URL', () => {
      route.snapshot.queryParams = {};

      component.onPageChange(-2);

      expect(router.navigate).not.toHaveBeenCalledWith([], { queryParams: { page: -2 }});
    });

    it('when a page is 0 number do not change the URL', () => {
      route.snapshot.queryParams = {};

      component.onPageChange(0);

      expect(router.navigate).not.toHaveBeenCalledWith([], { queryParams: { page: 0 }});
    });

    it('when a page changes do not remove existing URL parameters', () => {
      route.snapshot.queryParams = { sortField: ['name'], sortDirection: ['ASC'] };

      component.onPageChange(2);

      expect(router.navigate).toHaveBeenCalledWith(
        [], { queryParams: { sortField: ['name'], sortDirection: ['ASC'], page: 2 }});
    });
  });

  describe('updateAllFilters', () => {
    it('when the URL is empty', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateAllFilters([]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateServiceGroupsFilters({filters: {
          status: undefined,
          sortField: 'percent_ok',
          page: 1,
          pageSize: 25,
          sortDirection: 'ASC',
          searchBar: []
        }}));
    }));

    it('when the URL has a parameter not involved, '
      + 'it should not be used in the filter', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateAllFilters([{type: 'not-used', text: 'bob'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateServiceGroupsFilters({filters: {
          status: undefined,
          sortField: 'percent_ok',
          sortDirection: 'ASC',
          page: 1,
          pageSize: 25,
          searchBar: []
        }}));
    }));

    it('when the URL has a status selected', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateAllFilters([{type: 'status', text: 'ok'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateServiceGroupsFilters({filters: {
          status: 'ok',
          sortField: 'percent_ok',
          sortDirection: 'ASC',
          page: 1,
          pageSize: 25,
          searchBar: []
        }}));
    }));

    it('when the URL has an invalid status selected', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateAllFilters([{type: 'status', text: 'wrong'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateServiceGroupsFilters({filters: {
          status: undefined,
          sortField: 'percent_ok',
          sortDirection: 'ASC',
          page: 1,
          pageSize: 25,
          searchBar: []
        }}));
    }));

    it('when a user navigates to a negative page use the default page number', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateAllFilters([{type: 'page', text: '-2'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateServiceGroupsFilters({filters: {
          status: undefined,
          sortField: 'percent_ok',
          sortDirection: 'ASC',
          page: 1,
          pageSize: 25,
          searchBar: []
        }}));
    }));
  });
});
