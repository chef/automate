import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { ControlDetailsActionTypes, ControlDetailsActions } from './control-details.action';
import { ControlDetail } from './control-details.model';

export interface ControlDetailsEntityState extends EntityState<ControlDetail> {
  controlDetailsStatus: any;
  controlDetailsList: {
    items: ControlDetail[]
  };
}

const GET_STATUS = 'controlDetailsState';

export const controlDetailsEntityAdapter: EntityAdapter<ControlDetail> =
  createEntityAdapter<ControlDetail>({
    selectId: (controlDetails: ControlDetail) => controlDetails.id
});

export const ControlDetailsEntityInitialState: ControlDetailsEntityState =
controlDetailsEntityAdapter.getInitialState(<ControlDetailsEntityState><unknown>{
    getAllStatus: EntityStatus.notLoaded
  });

export function controlDetailsEntityReducer(
  state: ControlDetailsEntityState = ControlDetailsEntityInitialState,
  action: ControlDetailsActions): ControlDetailsEntityState {

  switch (action.type) {
    case ControlDetailsActionTypes.GET:
      return set(GET_STATUS, EntityStatus.loading, controlDetailsEntityAdapter.removeAll(state));

    case ControlDetailsActionTypes.GET_SUCCESS:
      return pipe(
        set(GET_STATUS, EntityStatus.loadingSuccess),
        set('controlDetailsList.items', action.payload || [])
      )(state) as ControlDetailsEntityState;

    case ControlDetailsActionTypes.GET_FAILURE:
      return set(GET_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: ControlDetailsEntityState) => state.entities[id];
