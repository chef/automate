import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map, filter } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { HttpStatus } from 'app/types/types';
import { Type } from 'app/entities/notifications/notification.model';

import {
  CreateRole,
  CreateRoleSuccess,
  CreateRoleFailure,
  GetRoles,
  GetRolesSuccess,
  RolesSuccessPayload,
  GetRolesFailure,
  RoleActionTypes,
  GetRole,
  GetRoleSuccess,
  GetRoleFailure
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
  createRole$ = this.actions$.pipe(
    ofType(RoleActionTypes.CREATE),
    mergeMap(({ payload: { server_id, org_id, role } }: CreateRole) =>
      this.requests.createRole(server_id, org_id, role).pipe(
        map((resp) => new CreateRoleSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new CreateRoleFailure(error))))));

  @Effect()
  createRoleSuccess$ = this.actions$.pipe(
    ofType(RoleActionTypes.CREATE_SUCCESS),
    map(({ payload }: CreateRoleSuccess) => new CreateNotification({
      type: Type.info,
      message: `Created notification ${payload.name}.`
    })));

  @Effect()
  createRoleFailure$ = this.actions$.pipe(
    ofType(RoleActionTypes.CREATE_FAILURE),
    filter(({ payload }: CreateRoleFailure) => payload.status !== HttpStatus.CONFLICT),
    map(({ payload }: CreateRoleFailure) => new CreateNotification({
      type: Type.error,
      message: `Could not create notification: ${payload.error.error || payload}.`
    })));

}
