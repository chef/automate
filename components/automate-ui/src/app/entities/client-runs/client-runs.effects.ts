import { map, catchError, withLatestFrom, switchMap, mergeMap } from 'rxjs/operators';
import { of } from 'rxjs';
import { Injectable } from '@angular/core';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import {
  ClientRunsActionTypes,
  GetNodes,
  GetNodesFailure,
  GetNodesSuccess,
  GetNodeCountSuccess,
  GetNodeCountFailure,
  GetNodeCount,
  GetWorkflowEnabledSuccess,
  GetWorkflowEnabledFailure,
  GetNodeSuggestionsSuccess,
  GetNodeSuggestionsFailure,
  GetNodeSuggestions,
  DeleteNodes,
  DeleteNodesSuccess,
  DeleteNodesFailure
} from './client-runs.actions';
import { ClientRunsRequests } from './client-runs.requests';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from '../../ngrx.reducers';

@Injectable()
export class ClientRunsEffects {
  constructor(
    private actions$: Actions,
    private requests: ClientRunsRequests,
    private store: Store<NgrxStateAtom>
  ) {}

  fetchClientRunsNodes$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ClientRunsActionTypes.GET_NODES),
      withLatestFrom(this.store),
      switchMap(([_action, storeState]) => {
        return this.requests.getNodes(storeState.clientRunsEntity.nodeFilter).pipe(
        map(responseNodes => new GetNodesSuccess({ nodes: responseNodes })),
        catchError((error) => of(new GetNodesFailure(error))));
      })));

  fetchClientRunsNodeCount$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ClientRunsActionTypes.GET_NODES_COUNT),
      withLatestFrom(this.store),
      switchMap(([_action, storeState]) => {
        return this.requests.getNodeCount(storeState.clientRunsEntity.nodeFilter).pipe(
        map(responseNodeCount => new GetNodeCountSuccess({ nodeCount: responseNodeCount })),
        catchError((error) => of(new GetNodeCountFailure(error))));
      })));

  fetchWorkflowEnabled$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ClientRunsActionTypes.GET_WORKFLOW_ENABLED),
      mergeMap(() => this.requests.isWorkflowEnabled()),
      map(workflowEnabled => new GetWorkflowEnabledSuccess({ workflowEnabled })),
      catchError((error) => of(new GetWorkflowEnabledFailure(error)))
    ));

  fetchNodeSuggestions$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ClientRunsActionTypes.GET_NODE_SUGGESTIONS),
      withLatestFrom(this.store),
      switchMap(([action, storeState]) => {
        const getNodeSuggestions = action as GetNodeSuggestions;
        return this.requests.getSuggestions(
          getNodeSuggestions.payload.type, getNodeSuggestions.payload.text,
          storeState.clientRunsEntity.nodeFilter).pipe(
        map(nodeSuggestions => new GetNodeSuggestionsSuccess({ nodeSuggestions })),
        catchError((error) => of(new GetNodeSuggestionsFailure(error))));
      })));

  deleteNodes$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ClientRunsActionTypes.DELETE_NODES),
      mergeMap((action: DeleteNodes) =>
        this.requests.deleteNodes(action.payload.nodeIdsToDelete)),
      map(_success => new DeleteNodesSuccess()),
      catchError((error) => of(new DeleteNodesFailure(error)))
    ));

  updateNodeFilters$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ClientRunsActionTypes.UPDATE_NODES_FILTER),
      mergeMap(() => [ new GetNodes(), new GetNodeCount() ])));

  deleteNodesSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ClientRunsActionTypes.DELETE_NODES_SUCCESS),
      mergeMap(() => [ new GetNodes(), new GetNodeCount() ])));

}
