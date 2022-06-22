import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { RunlistActionTypes, RunlistActions } from './runlists.action';
import { Runlist } from './runlists.model';

export interface RunlistEntityState extends EntityState<Runlist> {
  runlistsStatus: EntityStatus;
  getAllStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';

export const runlistEntityAdapter: EntityAdapter<Runlist> =
  createEntityAdapter<Runlist>({
    selectId: (runlist: Runlist) => runlist.id
  });

export const RunlistEntityInitialState: RunlistEntityState =
  runlistEntityAdapter.getInitialState(<RunlistEntityState>{
    getAllStatus: EntityStatus.notLoaded
  });

export function runlistEntityReducer(
  state: RunlistEntityState = RunlistEntityInitialState,
  action: RunlistActions): RunlistEntityState {

  switch (action.type) {
    case RunlistActionTypes.GET_ALL:
      return set(
        GET_ALL_STATUS,
        EntityStatus.loading,
        runlistEntityAdapter.removeAll(state)
      ) as RunlistEntityState;

    case RunlistActionTypes.GET_ALL_SUCCESS:
       return set(GET_ALL_STATUS, EntityStatus.loadingSuccess,
        runlistEntityAdapter.addOne(action.payload, state));

    case RunlistActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}
