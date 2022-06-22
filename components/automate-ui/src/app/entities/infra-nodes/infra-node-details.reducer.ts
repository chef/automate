import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { NodeActionTypes, NodeActions } from './infra-nodes.actions';
import { InfraNode } from './infra-nodes.model';

export interface InfraNodeDetailsEntityState extends EntityState<InfraNode> {
  nodeStatus: EntityStatus;
  getStatus: EntityStatus;
}

const GET_STATUS = 'getStatus';

export const infraNodeDetailsEntityAdapter: EntityAdapter<InfraNode> =
createEntityAdapter<InfraNode>({
  selectId: (infraNode: InfraNode) => infraNode.name
});

export const InfraNodeEntityInitialState: InfraNodeDetailsEntityState =
  infraNodeDetailsEntityAdapter.getInitialState(<InfraNodeDetailsEntityState>{
    getStatus: EntityStatus.notLoaded
  });

export function infraNodeDetailsEntityReducer(
  state: InfraNodeDetailsEntityState = InfraNodeEntityInitialState,
  action: NodeActions): InfraNodeDetailsEntityState {

  switch (action.type) {
    case NodeActionTypes.GET:
      return set(
        GET_STATUS,
        EntityStatus.loading,
        infraNodeDetailsEntityAdapter.removeAll(state)
      ) as InfraNodeDetailsEntityState;

    case NodeActionTypes.GET_SUCCESS:
      return set(GET_STATUS, EntityStatus.loadingSuccess,
        infraNodeDetailsEntityAdapter.addOne(action.payload, state));

    case NodeActionTypes.GET_FAILURE:
      return set(GET_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}
