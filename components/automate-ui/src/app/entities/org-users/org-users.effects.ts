import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetUsers,
  GetUsersSuccess,
  GetUsersFailure,
  UsersSuccessPayload,
  OrgUsersActionTypes
} from './org-users.action';

import { OrgUserRequests } from './org-users.requests';

@Injectable()
export class OrgUserEffects {
  constructor(
    private actions$: Actions,
    private requests: OrgUserRequests
  ) { }

  getUsers$ = createEffect(() =>
    this.actions$.pipe(
    ofType(OrgUsersActionTypes.GET_ALL),
    mergeMap(({ payload: { server_id, org_id } }: GetUsers) =>
    this.requests.OrgUserRequests(server_id, org_id).pipe(
      map((resp: UsersSuccessPayload) => new GetUsersSuccess(resp)),
      catchError((error: HttpErrorResponse) =>
      observableOf(new GetUsersFailure(error)))
    ))));

  getUsersFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgUsersActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetUsersFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get Org users: ${msg || payload.error}`
        });
    })));
}
