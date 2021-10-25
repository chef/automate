import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe, unset } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { NodeCredentialActionTypes, NodeCredentialActions } from './node-credential.actions';
import { NodeCredential } from './node-credential.model';

export interface NodeCredentialEntityState extends EntityState<NodeCredential> {
  nodeCredentialsStatus: EntityStatus;
  saveStatus: EntityStatus;
  saveError: EntityStatus;
  getStatus: EntityStatus;
  getAllStatus: EntityStatus;
  updateStatus: EntityStatus;
  deleteStatus: EntityStatus;
  status: EntityStatus;
  total: number;
}

const GET_ALL_STATUS = 'getAllStatus';
const GET_STATUS = 'getStatus';
const SAVE_STATUS = 'saveStatus';
const SAVE_ERROR = 'saveError';
const UPDATE_STATUS = 'updateStatus';
const DELETE_STATUS = 'deleteStatus';


export const nodeCredentialEntityAdapter: EntityAdapter<NodeCredential> =
createEntityAdapter<NodeCredential>({
  selectId: (nodeCredential: NodeCredential) => nodeCredential.id
});

export const NodeCredentialEntityInitialState: NodeCredentialEntityState =
nodeCredentialEntityAdapter.getInitialState(<NodeCredentialEntityState>{
  saveStatus: EntityStatus.notLoaded,
  saveError: null,
  getStatus: EntityStatus.notLoaded,
  getAllStatus: EntityStatus.notLoaded,
  deleteStatus: EntityStatus.notLoaded,
  updateStatus: EntityStatus.notLoaded,
  status: EntityStatus.notLoaded,
  total: 0
});

export function nodeCredentialEntityReducer(
  state: NodeCredentialEntityState = NodeCredentialEntityInitialState,
  action: NodeCredentialActions): NodeCredentialEntityState {

  switch (action.type) {
    case NodeCredentialActionTypes.SEARCH:
      return set('status', EntityStatus.loading, state);

    case NodeCredentialActionTypes.SEARCH_SUCCESS:
      const totalCountState = set('total', action.payload.total, state);
      return set('status', EntityStatus.loadingSuccess,
        nodeCredentialEntityAdapter.setAll(action.payload.secrets, totalCountState));

    case NodeCredentialActionTypes.SEARCH_FAILURE:
      return set('status', EntityStatus.loadingFailure, state);
    case NodeCredentialActionTypes.GET_ALL: {
      return set(GET_ALL_STATUS, EntityStatus.loading, state);
    }

    case NodeCredentialActionTypes.GET: {
      return set(
        GET_STATUS,
        EntityStatus.notLoaded,
        state
      );
    }

    case NodeCredentialActionTypes.GET_SUCCESS: {
      return set(
        GET_STATUS,
        EntityStatus.loadingSuccess,
        nodeCredentialEntityAdapter.addOne(action.payload, state)
      );
    }

    case NodeCredentialActionTypes.GET_FAILURE: {
      return set(
        GET_STATUS,
        EntityStatus.loadingFailure,
        state
      );
    }

    case NodeCredentialActionTypes.RESET: {
      return set(
        GET_STATUS,
        EntityStatus.notLoaded,
        nodeCredentialEntityAdapter.removeAll(state)
      );
    }

    case NodeCredentialActionTypes.GET_ALL_SUCCESS: {
      return set(
        GET_ALL_STATUS,
        EntityStatus.loadingSuccess,
        nodeCredentialEntityAdapter.setAll(action.payload.nodeCredentials, state)
      );
    }

    case NodeCredentialActionTypes.GET_ALL_FAILURE: {
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);
    }

    case NodeCredentialActionTypes.CREATE: {
      return set(SAVE_STATUS, EntityStatus.loading, state);
    }

    case NodeCredentialActionTypes.CREATE_SUCCESS: {
      return pipe(
        unset(SAVE_ERROR),
        set(SAVE_STATUS, EntityStatus.loadingSuccess)
      )(nodeCredentialEntityAdapter.addOne(action.payload, state)) as NodeCredentialEntityState;
    }

    case NodeCredentialActionTypes.CREATE_FAILURE: {
      return pipe(
        set(SAVE_ERROR, action.payload),
        set(SAVE_STATUS, EntityStatus.loadingFailure)
      )(state) as NodeCredentialEntityState;
    }
    case NodeCredentialActionTypes.UPDATE:
      return set(UPDATE_STATUS, EntityStatus.loading, state);

    case NodeCredentialActionTypes.UPDATE_SUCCESS:
      return set(UPDATE_STATUS, EntityStatus.loadingSuccess,
        nodeCredentialEntityAdapter.updateOne({
          id: action.payload.id,
          changes: action.payload
        }, state));

    case NodeCredentialActionTypes.UPDATE_FAILURE:
      return set(UPDATE_STATUS, EntityStatus.loadingFailure, state);

      case NodeCredentialActionTypes.DELETE:
        return set(DELETE_STATUS, EntityStatus.loading, state) as NodeCredentialEntityState;

      case NodeCredentialActionTypes.DELETE_SUCCESS:
        return set(DELETE_STATUS, EntityStatus.loadingSuccess,
          nodeCredentialEntityAdapter.removeOne(action.payload.id, state)) as
          NodeCredentialEntityState;

      case NodeCredentialActionTypes.DELETE_FAILURE:
        return set(DELETE_STATUS, EntityStatus.loadingFailure, state) as
        NodeCredentialEntityState;

    default:
      return state;
  }
}
