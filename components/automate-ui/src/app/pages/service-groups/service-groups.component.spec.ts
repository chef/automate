import { StoreModule } from '@ngrx/store';
import { Router } from '@angular/router';
import { ServiceGroupsComponent  } from './service-groups.component';
import { ServiceStatusIconPipe } from '../../pipes/service-status-icon.pipe';
import { TestBed, fakeAsync, tick } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { serviceGroupEntityReducer } from 'app/entities/service-groups/service-groups.reducer';
import { UpdateServiceGroupFilters } from 'app/entities/service-groups/service-groups.actions';

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

  describe('updateAllFilters', () => {
    it('when the URL is empty', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateAllFilters([]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateServiceGroupFilters({filters: {
          status: undefined
        }}));
    }));

    it('when the URL has a parameter not involved, '
      + 'it should not be used in the filter', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateAllFilters([{type: 'not-used', text: 'bob'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateServiceGroupFilters({filters: {
          status: undefined
        }}));
    }));

    it('when the URL has a status selected', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateAllFilters([{type: 'status', text: 'ok'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateServiceGroupFilters({filters: {
          status: 'ok'
        }}));
    }));

    it('when the URL has an invalid status selected', fakeAsync(() => {
      spyOn(component.store, 'dispatch');

      component.updateAllFilters([{type: 'status', text: 'wrong'}]);

      expect(component.store.dispatch).toHaveBeenCalledWith(
        new UpdateServiceGroupFilters({filters: {
          status: undefined
        }}));
    }));
  });
});
