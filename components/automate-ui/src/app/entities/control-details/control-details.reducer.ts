import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from '../../entities/entities';
import { ControlDetailActionTypes, ControlDetailActions } from './control-details.action';
import { ControlDetail } from './control-details.model';

export interface ControlDetailEntityState extends EntityState<ControlDetail> {
  controlsList: [];
  controlDetailStatus: EntityStatus;
  controlDetailList: [
    ControlDetail[]
  ];
}

const controlList = [];
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
  action: ControlDetailActions | any): ControlDetailEntityState {

  switch (action.type) {
    case ControlDetailActionTypes.GET:
      return set(GET_STATUS, EntityStatus.loading, controlDetailEntityAdapter.removeAll(state));

    case ControlDetailActionTypes.GET_SUCCESS:
      controlList.push(action.payload);
      return pipe(
        set(GET_STATUS, EntityStatus.loadingSuccess),
        set('controlDetailList', action.payload || []),
        set('controlsList', controlList)
      )(state) as ControlDetailEntityState;

    case ControlDetailActionTypes.GET_FAILURE:
      return set(GET_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: ControlDetailEntityState) => state.entities[id];
