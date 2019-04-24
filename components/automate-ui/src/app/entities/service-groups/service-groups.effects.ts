import { mergeMap, map, catchError, withLatestFrom, switchMap } from 'rxjs/operators';
import { of } from 'rxjs';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from '../../ngrx.reducers';
import { Injectable } from '@angular/core';
import { Actions, ofType, Effect } from '@ngrx/effects';
import { HttpErrorResponse } from '@angular/common/http';

import { ServiceGroupsPayload,
  ServiceGroupHealthSummary,
  ServicesPayload
      } from './service-groups.model';
import { ServiceGroupEntityState } from './service-groups.reducer';
import {
  ServiceGroupsActionTypes,
  GetServiceGroups,
  GetServiceGroupsCounts,
  GetServiceGroupsCountsSuccess,
  GetServiceGroupsCountsFailure,
  GetServicesBySG,
  GetServicesBySGSuccess,
  GetServicesBySGFailure,
  GetServiceGroupsSuccess,
  GetServiceGroupsFailure
} from './service-groups.actions';
import { ServiceGroupsRequests } from './service-groups.requests';

@Injectable()
export class ServiceGroupsEffects {
  constructor(
    private actions$: Actions,
    private requests: ServiceGroupsRequests,
    private store: Store<NgrxStateAtom>
  ) {}

  @Effect()
  getServiceGroups$ = this.actions$.pipe(
    ofType(ServiceGroupsActionTypes.GET_SERVICE_GROUPS),
    withLatestFrom(this.store),
    switchMap(([_action, storeState]) => {
      const serviceGroupsState: ServiceGroupEntityState = storeState.serviceGroups;
      return this.requests.fetchServiceGroups(serviceGroupsState.filters).pipe(
        map((payload: ServiceGroupsPayload) => new GetServiceGroupsSuccess(payload)),
        catchError((error: HttpErrorResponse) => of(new GetServiceGroupsFailure(error)))
      );
    }));

  @Effect()
  updateServiceGroupsFilters$ = this.actions$.pipe(
      ofType(ServiceGroupsActionTypes.UPDATE_SERVICE_GROUPS_FILTER),
      mergeMap(() => [ new GetServiceGroups(), new GetServiceGroupsCounts() ]));

  @Effect()
  getServiceGroupsCounts$ = this.actions$.pipe(
      ofType(ServiceGroupsActionTypes.GET_SERVICE_GROUPS_COUNTS),
      withLatestFrom(this.store),
      switchMap(([_action]) => {
        return this.requests.fetchServiceGroupHealth().pipe(
        map((payload: ServiceGroupHealthSummary) => new GetServiceGroupsCountsSuccess(payload)),
        catchError((error: HttpErrorResponse) => of(new GetServiceGroupsCountsFailure(error)))
      );
      }));

  @Effect()
  updateSelectedServiceGroup$ = this.actions$.pipe(
    ofType(ServiceGroupsActionTypes.UPDATE_SELECTED_SERVICE_GROUP),
    mergeMap(() => [
      new GetServicesBySG()
    ]));

  @Effect()
  getServicesBySG$ = this.actions$.pipe(
    ofType(ServiceGroupsActionTypes.GET_SERVICES_BY_SERVICE_GROUP),
    withLatestFrom(this.store),
    switchMap(([_action, storeState]) => {
      const serviceGroupsState: ServiceGroupEntityState = storeState.serviceGroups;
      return this.requests.fetchServicesBySG(serviceGroupsState.servicesFilters).pipe(
        map((payload: ServicesPayload) => new GetServicesBySGSuccess(payload)),
        catchError((error: HttpErrorResponse) => of(new GetServicesBySGFailure(error)))
      );
    }));
}
