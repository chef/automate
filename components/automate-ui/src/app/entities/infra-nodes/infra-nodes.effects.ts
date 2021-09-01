import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map, filter } from 'rxjs/operators';
import { HttpStatus } from 'app/types/types';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetNodes,
  GetNodesSuccess,
  GetNodesFailure,
  GetNode,
  GetNodeSuccess,
  GetNodeFailure,
  UpdateNode,
  UpdateNodeSuccess,
  UpdateNodeFailure,
  UpdateNodeEnvironment,
  UpdateNodeEnvironmentSuccess,
  UpdateNodeEnvironmentFailure,
  UpdateNodeTags,
  UpdateNodeTagsSuccess,
  UpdateNodeTagsFailure,
  DeleteNode,
  DeleteNodeSuccess,
  DeleteNodeFailure,
  NodesSuccessPayload,
  UpdateNodeAttributes,
  UpdateNodeAttributesSuccess,
  UpdateNodeAttributesFailure,
  GetPolicyGroupNodes,
  GetPolicyGroupNodesSuccess,
  GetPolicyGroupNodesFailure,
  NodeActionTypes
} from './infra-nodes.actions';

import {
  InfraNodeRequests
} from './infra-nodes.requests';

import { InfraNode } from './infra-nodes.model';

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

  getNode$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeActionTypes.GET),
      mergeMap(({ payload: { server_id, org_id, name } }: GetNode) =>
        this.requests.getNode(server_id, org_id, name).pipe(
          map((resp) => new GetNodeSuccess(resp)),
          catchError((error: HttpErrorResponse) => observableOf(new GetNodeFailure(error)))))));

  getNodeFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeActionTypes.GET_FAILURE),
      map(({ payload }: GetNodeFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get node: ${msg || payload.error}`
        });
    })));

  updateNode$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeActionTypes.UPDATE),
      mergeMap(({ payload }: UpdateNode) =>
        this.requests.updateNode(payload).pipe(
          map((resp: InfraNode) => new UpdateNodeSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new UpdateNodeFailure(error)))))));

  updateNodeSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeActionTypes.UPDATE_SUCCESS),
      map(({ payload: node }: UpdateNodeSuccess) => new CreateNotification({
        type: Type.info,
        message: `Successfully updated node - ${node.name} .`
      }))));

  updateNodeFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeActionTypes.UPDATE_FAILURE),
      filter(({ payload }: UpdateNodeFailure) => payload.status !== HttpStatus.CONFLICT),
      map(({ payload }: UpdateNodeFailure) => new CreateNotification({
        type: Type.error,
        message: `Could not update node: ${payload.error.error || payload}.`
      }))));

  updateNodeEnvironment$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeActionTypes.UPDATE_ENVIRONMENT),
      mergeMap(({ payload }: UpdateNodeEnvironment) =>
        this.requests.updateNodeEnvironment(payload.node).pipe(
          map((resp) => new UpdateNodeEnvironmentSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new UpdateNodeEnvironmentFailure(error)))))));

  updateNodeEnvironmentSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeActionTypes.UPDATE_ENVIRONMENT_SUCCESS),
      map(({ }: UpdateNodeEnvironmentSuccess) => new CreateNotification({
        type: Type.info,
        message: 'Successfully updated node environment.'
      }))));

  updateNodeEnvironmentFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeActionTypes.UPDATE_ENVIRONMENT_FAILURE),
      filter(({ payload }: UpdateNodeEnvironmentFailure) => payload.status !== HttpStatus.CONFLICT),
      map(({ payload }: UpdateNodeEnvironmentFailure) => new CreateNotification({
        type: Type.error,
        message: `Could not update node environment: ${payload.error.error || payload}.`
      }))));

  updateNodeTags$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeActionTypes.UPDATE_TAGS),
      mergeMap(({ payload }: UpdateNodeTags) =>
        this.requests.updateNodeTags(payload.node).pipe(
          map((resp) => new UpdateNodeTagsSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new UpdateNodeTagsFailure(error)))))));

  updateNodeTagsSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeActionTypes.UPDATE_TAGS_SUCCESS),
      map(({ }: UpdateNodeTagsSuccess) => new CreateNotification({
        type: Type.info,
        message: 'Successfully updated node tags.'
      }))));

  updateNodeTagsFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeActionTypes.UPDATE_TAGS_FAILURE),
      filter(({ payload }: UpdateNodeTagsFailure) => payload.status !== HttpStatus.CONFLICT),
      map(({ payload }: UpdateNodeTagsFailure) => new CreateNotification({
        type: Type.error,
        message: `Could not update node tags: ${payload.error.error || payload}.`
      }))));

  updateNodeAttributes$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeActionTypes.UPDATE_ATTRIBUTES),
      mergeMap(({ payload }: UpdateNodeAttributes) =>
        this.requests.updateNodeAttributes(payload).pipe(
          map((resp) => new UpdateNodeAttributesSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new UpdateNodeAttributesFailure(error)))))));

  updateNodeAttributesSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeActionTypes.UPDATE_ATTRIBUTES_SUCCESS),
      map(( ) => new CreateNotification({
        type: Type.info,
        message: 'Successfully updated node attibutes.'
      }))));

  updateNodeAttributesFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeActionTypes.UPDATE_ATTRIBUTES_FAILURE),
      filter(({ payload }: UpdateNodeAttributesFailure) => payload.status !== HttpStatus.CONFLICT),
      map(({ payload }: UpdateNodeAttributesFailure) => new CreateNotification({
        type: Type.error,
        message: `Could not update node attibutes: ${payload.error.error || payload}.`
      }))));

  GetPolicyGroupNodes$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NodeActionTypes.GET_ALL_NODES),
      mergeMap(({payload}: GetPolicyGroupNodes) =>
      this.requests.getPolicyGroupNodes(payload).pipe(
        map((resp: NodesSuccessPayload) => new  GetPolicyGroupNodesSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new GetPolicyGroupNodesFailure(error)))))));

  GetPolicyGroupNodesFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NodeActionTypes.GET_ALL_NODES_FAILURE),
    map(({ payload }: GetPolicyGroupNodesFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get nodes: ${msg || payload.error}`
      });
    })));
}
