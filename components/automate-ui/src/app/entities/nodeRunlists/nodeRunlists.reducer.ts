import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { NodeRunlistActionTypes, NodeRunlistActions } from './nodeRunlists.action';
import { NodeRunlist } from './nodeRunlists.model';

export interface NodeRunlistEntityState extends EntityState<NodeRunlist> {
  nodeRunlistsStatus: EntityStatus;
  getAllStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';

export const nodeRunlistEntityAdapter: EntityAdapter<NodeRunlist> =
  createEntityAdapter<NodeRunlist>({
    selectId: (nodeRunlist: NodeRunlist) => nodeRunlist.id
  });

export const NodeRunlistEntityInitialState: NodeRunlistEntityState =
  nodeRunlistEntityAdapter.getInitialState(<NodeRunlistEntityState>{
    getAllStatus: EntityStatus.notLoaded
  });

export function nodeRunlistEntityReducer(
  state: NodeRunlistEntityState = NodeRunlistEntityInitialState,
  action: NodeRunlistActions): NodeRunlistEntityState {

  switch (action.type) {
    case NodeRunlistActionTypes.GET_ALL:
      return set(
        GET_ALL_STATUS,
        EntityStatus.loading,
        nodeRunlistEntityAdapter.removeAll(state)
      ) as NodeRunlistEntityState;

    case NodeRunlistActionTypes.GET_ALL_SUCCESS:
       return set(GET_ALL_STATUS, EntityStatus.loadingSuccess,
        nodeRunlistEntityAdapter.addOne(action.payload, state)) as NodeRunlistEntityState;

    case NodeRunlistActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: NodeRunlistEntityState) => state.entities[id];
