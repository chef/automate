import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';

import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetRolesForOrg,
  GetRolesSuccess,
  RolesSuccessPayload,
  GetRolesFailure,
  RoleActionTypes,
  GetRole,
  RoleSuccessPayload,
  GetRoleSuccess,
  GetRoleFailure
} from './infra-role.action';

import {
  InfraRoleRequests
} from './infra-role.requests';

@Injectable()
export class InfraRoleEffects {
  constructor(
    private actions$: Actions,
    private requests: InfraRoleRequests
  ) { }

  @Effect()
  getRolesForOrgs$ = this.actions$.pipe(
      ofType(RoleActionTypes.GET_ALL),
      mergeMap(({ payload: { server_id, org_id } }: GetRolesForOrg) =>
        this.requests.getRolesForOrgs(server_id, org_id).pipe(
          map((resp: RolesSuccessPayload) => new GetRolesSuccess(resp)),
          catchError((error: HttpErrorResponse) => observableOf(new GetRolesFailure(error))))));

  @Effect()
  getCookbooksFailure$ = this.actions$.pipe(
      ofType(RoleActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetRolesFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get cookbooks: ${msg || payload.error}`
        });
      }));


  @Effect()
  getRole$ = this.actions$.pipe(
      ofType(RoleActionTypes.GET),
      mergeMap(({ payload: { server_id, org_id, name } }: GetRole) =>
        this.requests.getRole(server_id, org_id, name).pipe(
          map((resp: RoleSuccessPayload) => new GetRoleSuccess(resp)),
          catchError((error: HttpErrorResponse) => observableOf(new GetRoleFailure(error))))));

 @Effect()
  getOrgFailure$ = this.actions$.pipe(
      ofType(RoleActionTypes.GET_FAILURE),
      map(({ payload }: GetRoleFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get role: ${msg || payload.error}`
        });
      }));

}
