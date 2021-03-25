import { mergeMap, map, catchError, withLatestFrom, switchMap } from 'rxjs/operators';
import { of } from 'rxjs';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from '../../ngrx.reducers';
import { Injectable } from '@angular/core';
import { Actions, ofType, createEffect } from '@ngrx/effects';
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
  GetServiceGroupsSuggestions,
  DeleteServicesById,
  DeleteServicesByIdSuccess,
  DeleteServicesByIdFailure
} from './service-groups.actions';

import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

@Injectable()
export class ServiceGroupsEffects {
  constructor(
    private actions$: Actions,
    private requests: ServiceGroupsRequests,
    private store: Store<NgrxStateAtom>
  ) {}

  getServiceGroups$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ServiceGroupsActionTypes.GET_SERVICE_GROUPS),
    withLatestFrom(this.store.select(serviceGroupsFilters)),
    switchMap(([_action, filters]: [any, ServiceGroupsFilters]) => {
      return this.requests.fetchServiceGroups(filters).pipe(
        map((payload: ServiceGroupsPayload) => new GetServiceGroupsSuccess(payload)),
        catchError((error: HttpErrorResponse) => of(new GetServiceGroupsFailure(error)))
      );
    })));

  updateServiceGroupsFilters$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ServiceGroupsActionTypes.UPDATE_SERVICE_GROUPS_FILTER),
      mergeMap(() => [ new GetServiceGroups(), new GetServiceGroupsCounts() ])));

  getServiceGroupsCounts$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ServiceGroupsActionTypes.GET_SERVICE_GROUPS_COUNTS),
      withLatestFrom(this.store.select(serviceGroupsFilters)),
      switchMap(([_action, filters]: [any, ServiceGroupsFilters]) => {
        return this.requests.fetchServiceGroupHealth(filters).pipe(
          map((payload: ServiceGroupsHealthSummary) => new GetServiceGroupsCountsSuccess(payload)),
          catchError((error: HttpErrorResponse) => of(new GetServiceGroupsCountsFailure(error)))
        );
      })));

  updateSelectedServiceGroups$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ServiceGroupsActionTypes.UPDATE_SELECTED_SERVICE_GROUP),
    mergeMap(() => [
      new GetServicesBySG()
    ])));

  getServicesBySG$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ServiceGroupsActionTypes.GET_SERVICES_BY_SERVICE_GROUP),
    withLatestFrom(this.store.select(selectedServiceGroupFilters)),
    switchMap(([_action, filters]: [any, GroupServicesFilters]) => {
      return this.requests.fetchServicesBySG(filters).pipe(
        map((payload: GroupServicesPayload) => new GetServicesBySGSuccess(payload)),
        catchError((error: HttpErrorResponse) => of(new GetServicesBySGFailure(error)))
      );
    })));

  fetchNodeSuggestions$ = createEffect(() =>
    this.actions$.pipe(
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
      })));

  deleteServicesById$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ServiceGroupsActionTypes.DELETE_SERVICES_BY_ID),
      mergeMap((action: DeleteServicesById) =>
        this.requests.deleteServicesById(action.payload.servicesToDelete).pipe(
          map((response: GroupServicesPayload) =>
            new DeleteServicesByIdSuccess({ amount: response.services.length })),
          catchError((error: HttpErrorResponse) => of(new DeleteServicesByIdFailure(error))
          ))
      )));

  deleteServicesByIdFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ServiceGroupsActionTypes.DELETE_SERVICES_BY_ID_FAILURE),
    map((action: DeleteServicesByIdFailure) => {
      const msg = `Could not delete service: ${action.payload.error}`;
      return new CreateNotification({
        type: Type.error,
        message: msg
      });
    })
  ));

  deleteServicesByIdSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ServiceGroupsActionTypes.DELETE_SERVICES_BY_ID_SUCCESS),
      mergeMap((action: DeleteServicesByIdSuccess) => {
        const amount = action.payload.amount;
        const msg = amount === 1 ? '1 service deleted.' : `${amount} services deleted.`;
        return [
          new GetServiceGroups(),
          new GetServiceGroupsCounts(),
          new CreateNotification({
            type: Type.info,
            message: msg
          })
        ];
      })));
}
