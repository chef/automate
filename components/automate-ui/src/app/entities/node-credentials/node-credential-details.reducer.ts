import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { NodeCredentialActionTypes, NodeCredentialActions } from './node-credential.actions';
import { NodeCredential } from './node-credential.model';

export interface NodeCredentialDetailsEntityState extends EntityState<NodeCredential> {
  nodeCredentialStatus: EntityStatus;
  getStatus: EntityStatus;
}

const GET_STATUS = 'getStatus';

export const nodeCredentialDetailsEntityAdapter: EntityAdapter<NodeCredential> =
createEntityAdapter<NodeCredential>({
  selectId: (nodeCredential: NodeCredential) => nodeCredential.id
});

export const NodeCredentialEntityInitialState: NodeCredentialDetailsEntityState =
nodeCredentialDetailsEntityAdapter.getInitialState(<NodeCredentialDetailsEntityState>{
  getStatus: EntityStatus.notLoaded
});

export function nodeCredentialDetailsEntityReducer(
  state: NodeCredentialDetailsEntityState = NodeCredentialEntityInitialState,
  action: NodeCredentialActions): NodeCredentialDetailsEntityState {

  switch (action.type) {
    case NodeCredentialActionTypes.GET:
      return set(
        GET_STATUS,
        EntityStatus.loading,
        nodeCredentialDetailsEntityAdapter.removeAll(state)
      ) as NodeCredentialDetailsEntityState;

    case NodeCredentialActionTypes.GET_SUCCESS:
       return set(GET_STATUS, EntityStatus.loadingSuccess,
        nodeCredentialDetailsEntityAdapter.addOne(action.payload, state));
    case NodeCredentialActionTypes.GET_FAILURE:
      return set(GET_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}
