import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  NodeRunlistActionTypes,
  GetNodeRunlists,
  GetNodeRunlistsSuccess,
  GetNodeRunlistsFailure
} from './nodeRunlists.action';

import { NodeRunlistRequests } from './nodeRunlists.requests';

@Injectable()
export class NodeRunlistEffects {
  constructor(
    private actions$: Actions,
    private requests: NodeRunlistRequests
  ) { }

  getNodeRunlists$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeRunlistActionTypes.GET_ALL),
      mergeMap(({ payload: { server_id, org_id, name, id } }: GetNodeRunlists) =>
        this.requests.getNodeRunlist(server_id, org_id, name, id).pipe(
          map((resp) => new GetNodeRunlistsSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new GetNodeRunlistsFailure(error)))))));

  getNodeRunlistsFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeRunlistActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetNodeRunlistsFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get node run list: ${msg || payload.error}`
        });
    })));
}
