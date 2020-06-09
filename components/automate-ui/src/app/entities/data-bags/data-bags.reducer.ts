import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { DataBagsActionTypes, DataBagsActions } from './data-bags.actions';
import { DataBags } from './data-bags.model';

export interface DataBagsEntityState extends EntityState<DataBags> {
  getAllStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';

export const dataBagsEntityAdapter: EntityAdapter<DataBags> = createEntityAdapter<DataBags>({
  selectId: (dataBags: DataBags) => dataBags.name
});

export const DataBagsEntityInitialState: DataBagsEntityState =
dataBagsEntityAdapter.getInitialState(<DataBagsEntityState>{
  getAllStatus: EntityStatus.notLoaded
});

export function dataBagsEntityReducer(
  state: DataBagsEntityState = DataBagsEntityInitialState,
  action: DataBagsActions): DataBagsEntityState {

  switch (action.type) {
    case DataBagsActionTypes.GET_ALL:
      return set(GET_ALL_STATUS, EntityStatus.loading, dataBagsEntityAdapter.removeAll(state));

    case DataBagsActionTypes.GET_ALL_SUCCESS:
      return pipe(
        set(GET_ALL_STATUS, EntityStatus.loadingSuccess))
        (dataBagsEntityAdapter.setAll(action.payload.data_bags, state)) as DataBagsEntityState;

    case DataBagsActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: DataBagsEntityState) => state.entities[id];
