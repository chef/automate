import { Injectable } from '@angular/core';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { HttpErrorResponse } from '@angular/common/http';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { of } from 'rxjs';

import { NodesActionTypes, ListNodes, ListNodesSuccess, ListNodesFailure } from './nodes.actions';
import { RespNodes, NodesRequests } from './nodes.requests';
import { CreateNotification } from '../notifications/notification.actions';
import { Type } from '../notifications/notification.model';

@Injectable()
export class NodesEffects {
  constructor(
    private actions$: Actions,
    private requests: NodesRequests
  ) { }

  listNodes$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NodesActionTypes.LIST_NODES),
    mergeMap((action: ListNodes) =>
              this.requests.listNodes(action.payload).pipe(
              map((resp: RespNodes) =>
                   new ListNodesSuccess(resp)),
              catchError((error: HttpErrorResponse) =>
                     of(new ListNodesFailure(error)))))));

  listNodesFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodesActionTypes.LIST_NODES_FAILURE),
      map(({ payload }: ListNodesFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get nodes: ${msg || payload.error}`
        });
      })));

}
