import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';
import {
  RoleEnvironmentActionTypes,
  GetRoleEnvironmentsSuccess,
  GetRoleEnvironments,
  GetRoleEnvironmentsFailure
} from './role-environments.action';
import { RoleEnvironmentRequests } from './role-environments.requests';

@Injectable()
export class RoleEnvironmentEffects {
  constructor(
    private actions$: Actions,
    private requests: RoleEnvironmentRequests
  ) { }

  getRoleEnvironments$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RoleEnvironmentActionTypes.GET_ALL),
      mergeMap(({ payload: { server_id, org_id, name } }: GetRoleEnvironments) =>
        this.requests.getRoleEnvironments(server_id, org_id, name).pipe(
          map((resp) => new GetRoleEnvironmentsSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
          observableOf(new GetRoleEnvironmentsFailure(error)))))));

  getRoleEnvironmentsFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RoleEnvironmentActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetRoleEnvironmentsFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get environments: ${msg || payload.error}`
        });
      })));
}
