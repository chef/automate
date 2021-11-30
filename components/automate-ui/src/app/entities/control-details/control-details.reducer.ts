import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { ControlDetailActionTypes, ControlDetailActions } from './control-details.action';
import { ControlDetail } from './control-details.model';

export interface ControlDetailEntityState extends EntityState<ControlDetail> {
  controlDetailStatus: EntityStatus;
  controlDetailList: {
    items: ControlDetail[]
  };
}

const GET_STATUS = 'controlDetailStatus';

export const controlDetailEntityAdapter: EntityAdapter<ControlDetail> =
  createEntityAdapter<ControlDetail>({
    selectId: (controlDetail: ControlDetail) => controlDetail.id
});

export const ControlDetailEntityInitialState: ControlDetailEntityState =
  controlDetailEntityAdapter.getInitialState(<ControlDetailEntityState>{
    controlDetailStatus: EntityStatus.notLoaded
  });

export function controlDetailEntityReducer(
  state: ControlDetailEntityState = ControlDetailEntityInitialState,
  action: ControlDetailActions): ControlDetailEntityState {

  switch (action.type) {
    case ControlDetailActionTypes.GET:
      return set(GET_STATUS, EntityStatus.loading, controlDetailEntityAdapter.removeAll(state));

    case ControlDetailActionTypes.GET_SUCCESS:
      return pipe(
        set(GET_STATUS, EntityStatus.loadingSuccess),
        set('controlDetailList.items', action.payload || [])
      )(state) as ControlDetailEntityState;

    case ControlDetailActionTypes.GET_FAILURE:
      return set(GET_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: ControlDetailEntityState) => state.entities[id];
