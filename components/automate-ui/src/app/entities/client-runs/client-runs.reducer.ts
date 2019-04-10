import { HttpErrorResponse } from '@angular/common/http';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from '../entities';
import { Node, NodeFilter, ColumnsPreference } from './client-runs.model';
import { ClientRunsActionTypes, ClientRunsActions } from './client-runs.actions';
import { NodeCount } from '../../types/types';

export interface ClientRunsEntityState {
  nodes: Node[];
  nodeCount: NodeCount;
  workflowEnabled: boolean;
  nodeSuggestions: any[];
  status: EntityStatus;
  countStatus: EntityStatus;
  workflowStatus: EntityStatus;
  nodeSuggestionsStatus: EntityStatus;
  nodeDeleteStatus: EntityStatus;
  nodeFilter: NodeFilter;
  errorResp: HttpErrorResponse;
  columns: ColumnsPreference;
}

export const ClientRunsEntityInitialState: ClientRunsEntityState = {
  nodes: [],
  nodeCount: { total: 0, success: 0, failure: 0, missing: 0 },
  workflowEnabled: false,
  nodeSuggestions: [],
  status: EntityStatus.notLoaded,
  countStatus: EntityStatus.notLoaded,
  workflowStatus: EntityStatus.notLoaded,
  nodeSuggestionsStatus: EntityStatus.notLoaded,
  nodeDeleteStatus: EntityStatus.notLoaded,
  nodeFilter: {
    page: 0,
    pageSize: 100,
    sortDirection: 'ASC',
    sortField: 'name'
  },
  errorResp: null,
  columns: {
    check_in: true,
    uptime: true,
    platform: true,
    environment: true,
    policy_group: true,
    chef_version: false,
    deprecations_count: false
  }
};

export function clientRunsEntityReducer(
      state: ClientRunsEntityState = ClientRunsEntityInitialState,
      action: ClientRunsActions) {

  switch (action.type) {

    case ClientRunsActionTypes.GET_NODES:
      return set('status', EntityStatus.loading, state);

    case ClientRunsActionTypes.GET_NODES_SUCCESS:
      return pipe(
        set('status', EntityStatus.loadingSuccess),
        set('nodes', action.payload.nodes))(state);

    case ClientRunsActionTypes.GET_NODES_FAILURE:
      return pipe(
        set('status', EntityStatus.loadingFailure),
        set('errorResp', action.payload))(state);

    case ClientRunsActionTypes.GET_NODES_COUNT:
      return set('countStatus', EntityStatus.loading, state);

    case ClientRunsActionTypes.GET_NODES_COUNT_SUCCESS:
      return pipe(
        set('countStatus', EntityStatus.loadingSuccess),
        set('nodeCount', action.payload.nodeCount))(state);

    case ClientRunsActionTypes.GET_NODES_COUNT_FAILURE:
      return pipe(
        set('countStatus', EntityStatus.loadingFailure),
        set('errorResp', action.payload))(state);

    case ClientRunsActionTypes.GET_WORKFLOW_ENABLED:
      return set('workflowStatus', EntityStatus.loading, state);

    case ClientRunsActionTypes.GET_WORKFLOW_ENABLED_SUCCESS:
      return pipe(
        set('workflowStatus', EntityStatus.loadingSuccess),
        set('workflowEnabled', action.payload.workflowEnabled))(state);

    case ClientRunsActionTypes.GET_WORKFLOW_ENABLED_FAILURE:
      return pipe(
        set('workflowStatus', EntityStatus.loadingFailure),
        set('errorResp', action.payload))(state);

    case ClientRunsActionTypes.GET_NODE_SUGGESTIONS:
      return pipe(
        set('nodeSuggestionsStatus', EntityStatus.loading),
        set('nodeSuggestions', []))(state);

    case ClientRunsActionTypes.GET_NODE_SUGGESTIONS_SUCCESS:
      return pipe(
        set('nodeSuggestionsStatus', EntityStatus.loadingSuccess),
        set('nodeSuggestions', action.payload.nodeSuggestions))(state);

    case ClientRunsActionTypes.GET_NODE_SUGGESTIONS_FAILURE:
      return pipe(
        set('nodeSuggestions', []),
        set('nodeSuggestionsStatus', EntityStatus.loadingFailure),
        set('errorResp', action.payload))(state);

    case ClientRunsActionTypes.DELETE_NODES:
      return set('nodeDeleteStatus', EntityStatus.loading, state);

    case ClientRunsActionTypes.DELETE_NODES_SUCCESS:
      return set('nodeDeleteStatus', EntityStatus.loadingSuccess, state);

    case ClientRunsActionTypes.DELETE_NODES_FAILURE:
      return pipe(
        set('nodeDeleteStatus', EntityStatus.loadingFailure),
        set('errorResp', action.payload))(state);

    case ClientRunsActionTypes.UPDATE_NODES_FILTER: {
      const {filters: filters} = action.payload;
      return set('nodeFilter', filters)(state);
    }

    case ClientRunsActionTypes.GET_COLUMNS: {
      return set('columns', action.payload, state);
    }

    case ClientRunsActionTypes.UPDATE_COLUMNS: {
      return set('columns', action.payload, state);
    }

    default:
      return state;

  }
}
