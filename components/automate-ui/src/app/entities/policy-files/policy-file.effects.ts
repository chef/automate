import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetPolicyFiles,
  GetPolicyFilesSuccess,
  PolicyFilesSuccessPayload,
  GetPolicyFilesFailure,
  PolicyFileActionTypes
} from './policy-file.action';

import { PolicyFileRequests } from './policy-file.requests';

@Injectable()
export class PolicyFileEffects {
  constructor(
    private actions$: Actions,
    private requests: PolicyFileRequests
  ) { }

  getPolicyFiles$ = createEffect(() => {
    return this.actions$.pipe(
      ofType(PolicyFileActionTypes.GET_ALL),
      mergeMap(({ payload: { server_id, org_id } }: GetPolicyFiles) =>
        this.requests.getPolicyFiles(server_id, org_id).pipe(
          map((resp: PolicyFilesSuccessPayload) => new GetPolicyFilesSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
          observableOf(new GetPolicyFilesFailure(error))))));
  });

  getPolicyFilesFailure$ = createEffect(() => {
    return this.actions$.pipe(
      ofType(PolicyFileActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetPolicyFilesFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get policy files: ${msg || payload.error}`
        });
      }));
  });

}
