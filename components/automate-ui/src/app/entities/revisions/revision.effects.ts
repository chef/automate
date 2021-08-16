import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  RevisionActionTypes,
  GetRevisionsSuccess,
  GetRevisions,
  GetRevisionsFailure
} from './revision.action';

import { RevisionRequests } from './revision.requests';

@Injectable()
export class RevisionEffects {
  constructor(
    private actions$: Actions,
    private requests: RevisionRequests
  ) { }

  getRevisions$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RevisionActionTypes.GET_ALL),
      mergeMap(({ payload: { server_id, org_id, name } }: GetRevisions) =>
        this.requests.getRevisions(server_id, org_id, name).pipe(
          map((resp) => new GetRevisionsSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
          observableOf(new GetRevisionsFailure(error)))))));

  getRevisionsFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RevisionActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetRevisionsFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get revisions: ${msg || payload.error}`
        });
      })));
}
