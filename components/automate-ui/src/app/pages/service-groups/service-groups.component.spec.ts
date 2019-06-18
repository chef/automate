import { StoreModule, Store } from '@ngrx/store';
import { Router } from '@angular/router';
import { ServiceGroupsComponent  } from './service-groups.component';
import { ServiceStatusIconPipe } from '../../pipes/service-status-icon.pipe';
import { TestBed, fakeAsync, tick } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { serviceGroupEntityReducer } from 'app/entities/service-groups/service-groups.reducer';
import {
  UpdateServiceGroupFilters,
  GetServiceGroupsCountsSuccess
} from 'app/entities/service-groups/service-groups.actions';


describe('ServiceGroupsComponent', () => {
  let fixture, component;
  let router: Router;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        ServiceStatusIconPipe,
        ServiceGroupsComponent
      ],
      providers: [],
      imports: [
        StoreModule.forRoot({
          serviceGroups: serviceGroupEntityReducer
        }),
        RouterTestingModule
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(ServiceGroupsComponent);
    component = fixture.componentInstance;
    router = TestBed.get(Router);
    this.ngrxStore = TestBed.get(Store);
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
        this.ngrxStore.dispatch(new GetServiceGroupsCountsSuccess({
          total: 21,
          ok: 10,
          warning: 5,
          critical: 5,
          unknown: 1
        }));
      });

      it('should update the total number of service groups', fakeAsync(() => {
        expect(component.totalServiceGroups).toEqual(21);
      }));

      describe('and OK status filter update', () => {
        beforeEach(() => {
          this.ngrxStore.dispatch(new UpdateServiceGroupFilters({filters: {
            status: 'ok',
          }}));
        });

        it('should update the total number of service groups and selected status', fakeAsync(() => {
          expect(component.selectedStatus ).toEqual('ok');
          expect(component.totalServiceGroups).toEqual(10);
        }));
      });
    });
  });

  describe('statusFilter', () => {
    it('when set to an allowed value', () => {
      spyOn(component.router, 'navigate');
      component.statusFilter('critical');
      expect(component.router.navigate).toHaveBeenCalledWith(
        [], {queryParams: { status: ['critical'] }});
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

    it('when selection a different status the existing one should be replaced', fakeAsync(() => {
      router.navigate([''], {queryParams: { status: ['ok'] }});

      tick();

      component.statusFilter('unknown');

      tick();

      expect('/?status=unknown').toEqual(router.routerState.snapshot.url);
    }));
  });

  describe('onToggleSort', () => {
    it('when set to an allowed value', () => {
      spyOn(component.router, 'navigate');
      component.onToggleSort('name');
      expect(component.router.navigate).toHaveBeenCalledWith(
        [], {queryParams: { sortField: ['name'], sortDirection: ['ASC'] }});
    });

    it('when set to an allowed value', () => {
      spyOn(component.router, 'navigate');
      component.onToggleSort('percent_ok');
      expect(component.router.navigate).toHaveBeenCalledWith(
        [], {queryParams: { sortField: ['percent_ok'], sortDirection: ['ASC'] }});
    });
  });

  describe('onPageChange', () => {
    it('when the first page is selected remove page from URL', fakeAsync(() => {
      component.sgHealthSummary = { total: 30, ok: 30 };
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
      router.navigate([''], {queryParams: {sortField: ['name'], sortDirection: ['ASC']}});

      tick();

      component.onPageChange(2);

      tick();

      expect('/?sortField=name&sortDirection=ASC&page=2').
        toEqual(router.routerState.snapshot.url);
    }));
  });


  describe('updateAllFilters', () => {
    it('when the URL is empty', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateAllFilters([]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateServiceGroupFilters({filters: {
          status: undefined,
          sortField: 'percent_ok',
          sortDirection: 'ASC',
          page: 1,
          pageSize: 25
        }}));
    }));

    it('when the URL has a parameter not involved, '
      + 'it should not be used in the filter', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateAllFilters([{type: 'not-used', text: 'bob'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateServiceGroupFilters({filters: {
          status: undefined,
          sortField: 'percent_ok',
          sortDirection: 'ASC',
          page: 1,
          pageSize: 25
        }}));
    }));

    it('when the URL has a status selected', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateAllFilters([{type: 'status', text: 'ok'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateServiceGroupFilters({filters: {
          status: 'ok',
          sortField: 'percent_ok',
          sortDirection: 'ASC',
          page: 1,
          pageSize: 25
        }}));
    }));

    it('when the URL has an invalid status selected', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateAllFilters([{type: 'status', text: 'wrong'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateServiceGroupFilters({filters: {
          status: undefined,
          sortField: 'percent_ok',
          sortDirection: 'ASC',
          page: 1,
          pageSize: 25
        }}));
    }));

    it('when a user navigates to a negative page use the default page number', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateAllFilters([{type: 'page', text: '-2'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateServiceGroupFilters({filters: {
          status: undefined,
          sortField: 'percent_ok',
          sortDirection: 'ASC',
          page: 1,
          pageSize: 25
        }}));
    }));
  });
});
