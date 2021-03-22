import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetRolesSuccess,
  GetRolesSuccessPayload,
  GetRolesFailure,
  RoleActionTypes,
  GetRole,
  GetRoleSuccess,
  GetRoleFailure,
  DeleteRole,
  DeleteRoleSuccess,
  DeleteRoleFailure
} from './role.actions';

import {
  RoleRequests,
  RoleResponse
} from './role.requests';

@Injectable()
export class RoleEffects {
  constructor(
    private actions$: Actions,
    private requests: RoleRequests
  ) { }

  getRoles$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RoleActionTypes.GET_ALL),
      mergeMap(() =>
        this.requests.getRoles().pipe(
          map((resp: GetRolesSuccessPayload) => new GetRolesSuccess(resp)),
          catchError((error: HttpErrorResponse) => observableOf(new GetRolesFailure(error)))))));

  getRolesFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RoleActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetRolesFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get roles: ${msg || payload.error}`
        });
      })));

  getRole$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RoleActionTypes.GET),
      mergeMap(({ payload: { id }}: GetRole) =>
        this.requests.getRole(id).pipe(
          map((resp: RoleResponse) => new GetRoleSuccess(resp.role)),
          catchError((error: HttpErrorResponse) => observableOf(new GetRoleFailure(error, id)))))));

  getRoleFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RoleActionTypes.GET_FAILURE),
      map(({ payload, id }: GetRoleFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get role ${id}: ${msg || payload.error}`
        });
      })));

  deleteRole$ = createEffect(() =>
    this.actions$.pipe(
    ofType(RoleActionTypes.DELETE),
    mergeMap(({ payload: { id} }: DeleteRole) =>
      this.requests.deleteRole(id).pipe(
        map(() => new DeleteRoleSuccess({id})),
        catchError((error: HttpErrorResponse) =>
          observableOf(new DeleteRoleFailure(error)))))));

  deleteRoleSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RoleActionTypes.DELETE_SUCCESS),
      map(({ payload: { id } }: DeleteRoleSuccess) => {
        return new CreateNotification({
          type: Type.info,
          message: `Deleted role ${id}.`
        });
      })));

  deleteRoleFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(RoleActionTypes.DELETE_FAILURE),
    map(({ payload: { error } }: DeleteRoleFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not delete role: ${msg || error}`
      });
    })));

}
