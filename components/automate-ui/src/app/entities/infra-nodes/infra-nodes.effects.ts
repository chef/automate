import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';

import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetNodes,
  GetNodesSuccess,
  NodesSuccessPayload,
  GetNodesFailure,
  NodeActionTypes
} from './infra-nodes.actions';

import {
  NodeRequests
} from './infra-nodes.requests';

@Injectable()
export class InfraNodeEffects {
  constructor(
    private actions$: Actions,
    private requests: NodeRequests
  ) { }

  @Effect()
  getNodes$ = this.actions$.pipe(
      ofType(NodeActionTypes.GET_ALL),
      mergeMap(({ payload: { server_id, org_id } }: GetNodes) =>
        this.requests.getNodes(server_id, org_id).pipe(
          map((resp: NodesSuccessPayload) => new GetNodesSuccess(resp)),
          catchError(
            (error: HttpErrorResponse) => observableOf(new GetNodesFailure(error)
            )))));

  @Effect()
  getNodesFailure$ = this.actions$.pipe(
      ofType(NodeActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetNodesFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get nodes: ${msg || payload.error}`
        });
      }));

}
