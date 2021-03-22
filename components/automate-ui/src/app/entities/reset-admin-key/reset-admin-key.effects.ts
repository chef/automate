import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';

import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  UpdateAdminKey,
  UpdateAdminKeyFailure,
  UpdateAdminKeySuccess,
  AdminKeyActionTypes
} from './reset-admin-key.actions';

import {
  AdminKeyRequests
} from './reset-admin-key.requests';

@Injectable()
export class AdminKeyEffects {
  constructor(
    private actions$: Actions,
    private requests: AdminKeyRequests
  ) { }

  updateAdminKey$ = createEffect(() =>
    this.actions$.pipe(
      ofType(AdminKeyActionTypes.UPDATE),
      mergeMap(({ payload: {server_id, org_id, admin_key} }: UpdateAdminKey) =>
        this.requests.updateAdminKey(server_id, org_id, admin_key).pipe(
          map((resp) => new UpdateAdminKeySuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new UpdateAdminKeyFailure(error)))))));

  updateAdminKeySuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(AdminKeyActionTypes.UPDATE_SUCCESS),
      map(({ payload }: UpdateAdminKeySuccess) => new CreateNotification({
      type: Type.info,
      message: `Reset admin key for organization ${payload.org.name}.`
    }))));

  updateAdminKeyFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(AdminKeyActionTypes.UPDATE_FAILURE),
      map(({ payload }: UpdateAdminKeyFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not update organization admin key: ${msg || payload.error}`
        });
      })));

}
