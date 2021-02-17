import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetRoles,
  GetRolesSuccess,
  RolesSuccessPayload,
  GetRolesFailure,
  RoleActionTypes,
  GetRole,
  GetRoleSuccess,
  GetRoleFailure,
  RoleSearch,
  RoleSearchSuccess,
  RoleSearchFailure,
  RoleSearchSuccessPayload
} from './infra-role.action';

import { InfraRoleRequests } from './infra-role.requests';

@Injectable()
export class InfraRoleEffects {
  constructor(
    private actions$: Actions,
    private requests: InfraRoleRequests
  ) { }

  @Effect()
  getRoles$ = this.actions$.pipe(
    ofType(RoleActionTypes.GET_ALL),
    mergeMap(({ payload: { server_id, org_id } }: GetRoles) =>
      this.requests.getRoles(server_id, org_id).pipe(
        map((resp: RolesSuccessPayload) => new GetRolesSuccess(resp)),
        catchError((error: HttpErrorResponse) => observableOf(new GetRolesFailure(error))))));

  @Effect()
  getRolesFailure$ = this.actions$.pipe(
    ofType(RoleActionTypes.GET_ALL_FAILURE),
    map(({ payload }: GetRolesFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get infra roles: ${msg || payload.error}`
      });
    }));

  @Effect()
  getRole$ = this.actions$.pipe(
    ofType(RoleActionTypes.GET),
    mergeMap(({ payload: { server_id, org_id, name } }: GetRole) =>
      this.requests.getRole(server_id, org_id, name).pipe(
        map((resp) => new GetRoleSuccess(resp)),
        catchError((error: HttpErrorResponse) => observableOf(new GetRoleFailure(error))))));

  @Effect()
  getRoleFailure$ = this.actions$.pipe(
    ofType(RoleActionTypes.GET_FAILURE),
    map(({ payload }: GetRoleFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get infra role: ${msg || payload.error}`
      });
    }));


  @Effect()
  getRoleSearchDetails$ = this.actions$.pipe(
    ofType(RoleActionTypes.SEARCH),
    mergeMap((action: RoleSearch) =>
      this.requests.getRoleSearch(action.payload).pipe(
        map((resp: RoleSearchSuccessPayload) => new RoleSearchSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new RoleSearchFailure(error))))));

  @Effect()
  getRoleSearchDetailsFailure$ = this.actions$.pipe(
    ofType(RoleActionTypes.SEARCH_FAILURE),
    map(({ payload }: RoleSearchFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get infra role: ${msg || payload.error}`
      });
    }));


}
