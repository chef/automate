import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf, of } from 'rxjs';
import { catchError, mergeMap, map, filter } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  NodeCredentialActionTypes,
  GetNodeCredential,
  GetNodeCredentialSuccess,
  GetNodeCredentialFailure,
  CreateNodeCredential,
  CreateNodeCredentialSuccess,
  CreateNodeCredentialFailure,
  UpdateNodeCredential,
  UpdateNodeCredentialSuccess,
  UpdateNodeCredentialFailure,
  NodeCredentialsSearch,
  NodeCredentialsSearchSuccessPayload,
  NodeCredentialsSearchSuccess,
  NodeCredentialsSearchFailure,
  DeleteNodeCredential,
  DeleteNodeCredentialSuccess,
  DeleteNodeCredentialFailure
} from './node-credential.actions';

import { NodeCredentialRequests, NodeCredentialResponse } from './node-credential.requests';
import { HttpStatus } from 'app/types/types';
import { NodeCredential } from './node-credential.model';

@Injectable()
export class NodeCredentialEffects {
  constructor(
    private actions$: Actions,
    private requests: NodeCredentialRequests
  ) { }

  nodeCredentialsSearch$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NodeCredentialActionTypes.SEARCH),
    mergeMap((action: NodeCredentialsSearch) =>
      this.requests.getNodeCredentials(action.payload).pipe(
      map((payload: NodeCredentialsSearchSuccessPayload) =>
      new NodeCredentialsSearchSuccess(payload)),
      catchError((error: HttpErrorResponse) => of(new NodeCredentialsSearchFailure(error)))))));

  nodeCredentialsSearchFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeCredentialActionTypes.SEARCH_FAILURE),
      map(({ payload }: NodeCredentialsSearchFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not search node credentials: ${msg || payload.error}`
        });
      })));

  getNodeCredential$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NodeCredentialActionTypes.GET),
      mergeMap(({ payload: { id } }: GetNodeCredential) =>
      this.requests.getNodeCredential( id).pipe(
      map((resp) => new GetNodeCredentialSuccess(resp)),
      catchError((error: HttpErrorResponse) => observableOf(
        new GetNodeCredentialFailure(error)))))));

  getNodeCredentialFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeCredentialActionTypes.GET_FAILURE),
      map(({ payload }: GetNodeCredentialFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get node credential: ${msg || payload.error}`
        });
      })));

  createNodeCredential$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NodeCredentialActionTypes.CREATE),
    mergeMap(({ payload }: CreateNodeCredential) =>
      this.requests.createNodeCredential(payload).pipe(
      map((resp: NodeCredentialResponse) => new CreateNodeCredentialSuccess(resp)),
      catchError((error: HttpErrorResponse) =>
      observableOf(new CreateNodeCredentialFailure(error)))))));

  createNodeCredentialSuccess$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NodeCredentialActionTypes.CREATE_SUCCESS),
    map(({ payload: { } }: CreateNodeCredentialSuccess) => new CreateNotification({
      type: Type.info,
      message: 'Created node credential.'
    }))));

  createNodeCredentialFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NodeCredentialActionTypes.CREATE_FAILURE),
    filter(({ payload }: CreateNodeCredentialFailure) => payload.status !== HttpStatus.CONFLICT),
      map(({ payload }: CreateNodeCredentialFailure) => new CreateNotification({
        type: Type.error,
        message: `Could not create node credential: ${payload.error.error || payload}`
    }))));

  updateNodeCredential$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NodeCredentialActionTypes.UPDATE),
    mergeMap(({ payload  }: UpdateNodeCredential) =>
      this.requests.updateNodeCredential(payload).pipe(
        map((resp: NodeCredential) => new UpdateNodeCredentialSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new UpdateNodeCredentialFailure(error)))))));

  updateNodeCredentialFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NodeCredentialActionTypes.UPDATE_FAILURE),
    map(({ payload }: UpdateNodeCredentialFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not update node credential: ${msg || payload.error}`
      });
    })));

  deleteNodeCredential$ = createEffect(() =>
    this.actions$.pipe(
    ofType<DeleteNodeCredential>(NodeCredentialActionTypes.DELETE),
    mergeMap(({ payload }) =>
      this.requests.deleteNodeCredential(payload.id).pipe(
      map(() => new DeleteNodeCredentialSuccess(payload)),
      catchError((error: HttpErrorResponse) =>
      observableOf(new DeleteNodeCredentialFailure(error)))))));

  deleteNodeCredentialSuccess$ = createEffect(() =>
    this.actions$.pipe(
    ofType<DeleteNodeCredentialSuccess>(NodeCredentialActionTypes.DELETE_SUCCESS),
    map(({ payload: { name } }) => new CreateNotification({
      type: Type.info,
      message: `Deleted node credential ${name}.`
    }))));

  deleteNodeCredentialFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType<DeleteNodeCredentialFailure>(NodeCredentialActionTypes.DELETE_FAILURE),
    map(({ payload: { error } }) =>  new CreateNotification({
      type: Type.error,
      message: `Could not delete node credential: ${error.error || error}`
    }))));

}
