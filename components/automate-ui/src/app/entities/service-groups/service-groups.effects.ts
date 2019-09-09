import { mergeMap, map, catchError, withLatestFrom, switchMap } from 'rxjs/operators';
import { of } from 'rxjs';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from '../../ngrx.reducers';
import { Injectable } from '@angular/core';
import { Actions, ofType, Effect } from '@ngrx/effects';
import { HttpErrorResponse } from '@angular/common/http';

import { ServiceGroupsRequests } from './service-groups.requests';

import {
  ServiceGroupsPayload,
  ServiceGroupsHealthSummary,
  ServiceGroupsFilters,
  GroupServicesFilters,
  GroupServicesPayload
} from './service-groups.model';

import {
  serviceGroupsFilters,
  selectedServiceGroupFilters
} from './service-groups.selector';

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
  GetServiceGroupsFailure,
  GetServiceGroupsSuggestionsSuccess,
  GetServiceGroupsSuggestionsFailure,
  GetServiceGroupsSuggestions
} from './service-groups.actions';

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
    withLatestFrom(this.store.select(serviceGroupsFilters)),
    switchMap(([_action, filters]: [any, ServiceGroupsFilters]) => {
      return this.requests.fetchServiceGroups(filters).pipe(
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
        map((payload: ServiceGroupsHealthSummary) => new GetServiceGroupsCountsSuccess(payload)),
        catchError((error: HttpErrorResponse) => of(new GetServiceGroupsCountsFailure(error)))
      );
      }));

  @Effect()
  updateSelectedServiceGroups$ = this.actions$.pipe(
    ofType(ServiceGroupsActionTypes.UPDATE_SELECTED_SERVICE_GROUP),
    mergeMap(() => [
      new GetServicesBySG()
    ]));

  @Effect()
  getServicesBySG$ = this.actions$.pipe(
    ofType(ServiceGroupsActionTypes.GET_SERVICES_BY_SERVICE_GROUP),
    withLatestFrom(this.store.select(selectedServiceGroupFilters)),
    switchMap(([_action, filters]: [any, GroupServicesFilters]) => {
      return this.requests.fetchServicesBySG(filters).pipe(
        map((payload: GroupServicesPayload) => new GetServicesBySGSuccess(payload)),
        catchError((error: HttpErrorResponse) => of(new GetServicesBySGFailure(error)))
      );
    }));

  @Effect()
  fetchNodeSuggestions$ = this.actions$.pipe(
      ofType(ServiceGroupsActionTypes.GET_SERVICE_GROUPS_SUGGESTIONS),
      withLatestFrom(this.store.select(serviceGroupsFilters)),
      switchMap(([getServiceGroupsSuggestions, filters]:
        [GetServiceGroupsSuggestions, ServiceGroupsFilters]) => {
        return this.requests.getSuggestions(
          getServiceGroupsSuggestions.payload.type, getServiceGroupsSuggestions.payload.text,
          filters).pipe(
        map(serviceGroupsSuggestions =>
          new GetServiceGroupsSuggestionsSuccess({ serviceGroupsSuggestions })),
        catchError((error) => of(new GetServiceGroupsSuggestionsFailure(error))));
      }));
}
