import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';

import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetNodes,
  GetNodesSuccess,
  NodesSuccessPayload,
  GetNodesFailure,
  NodeActionTypes,
  DeleteNode,
  DeleteNodeSuccess,
  DeleteNodeFailure
} from './infra-nodes.actions';

import {
  InfraNodeRequests
} from './infra-nodes.requests';

@Injectable()
export class InfraNodeEffects {
  constructor(
    private actions$: Actions,
    private requests: InfraNodeRequests
  ) { }

  getNodes$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NodeActionTypes.GET_ALL),
    mergeMap((action: GetNodes) =>
      this.requests.getNodes(action.payload).pipe(
        map((resp: NodesSuccessPayload) => new GetNodesSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new GetNodesFailure(error)))))));

  getNodesFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NodeActionTypes.GET_ALL_FAILURE),
    map(({ payload }: GetNodesFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get nodes: ${msg || payload.error}`
      });
    })));

  deleteNode$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeActionTypes.DELETE),
      mergeMap(({ payload: { server_id, org_id, name } }: DeleteNode) =>
        this.requests.deleteNode(server_id, org_id, name).pipe(
          map(() => new DeleteNodeSuccess({ name })),
          catchError((error: HttpErrorResponse) =>
            observableOf(new DeleteNodeFailure(error)))))));

  deleteNodeSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeActionTypes.DELETE_SUCCESS),
      map(({ payload: { name } }: DeleteNodeSuccess) => {
        return new CreateNotification({
          type: Type.info,
          message: `Successfully deleted node - ${name}.`
        });
    })));

  deleteNodeFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeActionTypes.DELETE_FAILURE),
      map(({ payload: { error } }: DeleteNodeFailure) => {
        const msg = error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not delete node: ${msg || error}`
        });
    })));

}
