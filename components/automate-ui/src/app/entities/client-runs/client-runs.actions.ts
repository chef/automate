import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import {
  Node, ColumnsPreference
} from './client-runs.model';

import { NodeCount } from '../../types/types';


export enum ClientRunsActionTypes {
  GET_NODES = 'CLIENTRUNS::NODES::GET',
  GET_NODES_SUCCESS = 'CLIENTRUNS::NODES::GET::SUCCESS',
  GET_NODES_FAILURE = 'CLIENTRUNS::NODES::GET::FAILURE',
  GET_NODES_COUNT = 'CLIENTRUNS::NODECOUNT::GET',
  GET_NODES_COUNT_SUCCESS = 'CLIENTRUNS::NODECOUNT::GET::SUCCESS',
  GET_NODES_COUNT_FAILURE = 'CLIENTRUNS::NODECOUNT::GET::FAILURE',

  GET_WORKFLOW_ENABLED = 'CLIENTRUNS::GET_WORKFLOW_ENABLED::GET',
  GET_WORKFLOW_ENABLED_SUCCESS = 'CLIENTRUNS::GET_WORKFLOW_ENABLED::GET::SUCCESS',
  GET_WORKFLOW_ENABLED_FAILURE = 'CLIENTRUNS::GET_WORKFLOW_ENABLED::GET::FAILURE',

  GET_NODE_SUGGESTIONS = 'CLIENTRUNS::GET_NODE_SUGGESTIONS::GET',
  GET_NODE_SUGGESTIONS_SUCCESS = 'CLIENTRUNS::GET_NODE_SUGGESTIONS::GET::SUCCESS',
  GET_NODE_SUGGESTIONS_FAILURE = 'CLIENTRUNS::GET_NODE_SUGGESTIONS::GET::FAILURE',

  DELETE_NODES = 'CLIENTRUNS::NODES::DELETE',
  DELETE_NODES_SUCCESS = 'CLIENTRUNS::NODES::DELETE::SUCCESS',
  DELETE_NODES_FAILURE = 'CLIENTRUNS::NODES::DELETE::FAILURE',

  UPDATE_NODES_FILTER = 'CLIENTRUNS::NODE_FILTER::UPDATE',

  GET_COLUMNS = 'CLIENTRUNS::COLUMNS::GET',
  UPDATE_COLUMNS = 'CLIENTRUNS::COLUMNS::UPDATE'
}

export class GetNodes implements Action {
  readonly type = ClientRunsActionTypes.GET_NODES;
  constructor() {}
}

export class GetNodesSuccess implements Action {
  readonly type = ClientRunsActionTypes.GET_NODES_SUCCESS;

  constructor(public payload: { nodes: Node[] }) {}
}

export class GetNodesFailure implements Action {
  readonly type = ClientRunsActionTypes.GET_NODES_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class GetNodeCount implements Action {
  readonly type = ClientRunsActionTypes.GET_NODES_COUNT;
  constructor() {}
}

export class GetNodeCountSuccess implements Action {
  readonly type = ClientRunsActionTypes.GET_NODES_COUNT_SUCCESS;

  constructor(public payload: { nodeCount: NodeCount }) {}
}

export class GetNodeCountFailure implements Action {
  readonly type = ClientRunsActionTypes.GET_NODES_COUNT_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class GetWorkflowEnabled implements Action {
  readonly type = ClientRunsActionTypes.GET_WORKFLOW_ENABLED;
  constructor() {}
}

export class GetWorkflowEnabledSuccess implements Action {
  readonly type = ClientRunsActionTypes.GET_WORKFLOW_ENABLED_SUCCESS;

  constructor(public payload: { workflowEnabled: boolean }) {}
}

export class GetWorkflowEnabledFailure implements Action {
  readonly type = ClientRunsActionTypes.GET_WORKFLOW_ENABLED_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class GetNodeSuggestions implements Action {
  readonly type = ClientRunsActionTypes.GET_NODE_SUGGESTIONS;
  constructor(public payload: { type: string, text: string }) {}
}

export class GetNodeSuggestionsSuccess implements Action {
  readonly type = ClientRunsActionTypes.GET_NODE_SUGGESTIONS_SUCCESS;

  constructor(public payload: { nodeSuggestions: any[] }) {}
}

export class GetNodeSuggestionsFailure implements Action {
  readonly type = ClientRunsActionTypes.GET_NODE_SUGGESTIONS_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class DeleteNodes implements Action {
  readonly type = ClientRunsActionTypes.DELETE_NODES;
  constructor(public payload: { nodeIdsToDelete: string[] }) {}
}

export class DeleteNodesSuccess implements Action {
  readonly type = ClientRunsActionTypes.DELETE_NODES_SUCCESS;

  constructor( ) {}
}

export class DeleteNodesFailure implements Action {
  readonly type = ClientRunsActionTypes.DELETE_NODES_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateNodeFilters implements Action {
  readonly type = ClientRunsActionTypes.UPDATE_NODES_FILTER;

  constructor(public payload: {filters} ) {}
}

export class GetColumns implements Action {
  readonly type = ClientRunsActionTypes.GET_COLUMNS;
  constructor(public payload: ColumnsPreference) {} // todo update this with actual type
}


export class UpdateColumns implements Action {
  readonly type = ClientRunsActionTypes.UPDATE_COLUMNS;
  constructor(public payload: ColumnsPreference) {} // todo update this with actual type
}

export type ClientRunsActions =
  | GetNodesSuccess
  | GetNodesFailure
  | GetNodes
  | GetNodeCount
  | GetNodeCountSuccess
  | GetNodeCountFailure
  | GetWorkflowEnabled
  | GetWorkflowEnabledSuccess
  | GetWorkflowEnabledFailure
  | GetNodeSuggestions
  | GetNodeSuggestionsSuccess
  | GetNodeSuggestionsFailure
  | DeleteNodes
  | DeleteNodesSuccess
  | DeleteNodesFailure
  | UpdateNodeFilters
  | GetColumns
  | UpdateColumns;
