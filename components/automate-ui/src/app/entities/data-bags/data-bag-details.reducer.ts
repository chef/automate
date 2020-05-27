import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { DataBagDetailsActionTypes, DataBagDetailsActions } from './data-bag-details.action';
import { DataBags } from './data-bags.model';

export interface DataBagDetailsEntityState extends EntityState<DataBags> {
  getAllStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';

export const dataBagDetailsEntityAdapter: EntityAdapter<DataBags> = createEntityAdapter<DataBags>({
  selectId: (dataBags: DataBags) => dataBags.name
});

export const DataBagsEntityInitialState: DataBagDetailsEntityState =
dataBagDetailsEntityAdapter.getInitialState(<DataBagDetailsEntityState>{
    getAllStatus: EntityStatus.notLoaded
  });

export function dataBagDetailsEntityReducer(
  state: DataBagDetailsEntityState = DataBagsEntityInitialState,
  action: DataBagDetailsActions): DataBagDetailsEntityState {

  switch (action.type) {
    case DataBagDetailsActionTypes.GET_ALL:
      return set(GET_ALL_STATUS, EntityStatus.loading, dataBagDetailsEntityAdapter
        .removeAll(state));

    case DataBagDetailsActionTypes.GET_ALL_SUCCESS:
      return pipe(
        set(GET_ALL_STATUS, EntityStatus.loadingSuccess))
        (dataBagDetailsEntityAdapter
          .setAll(action.payload.data_bags, state)) as DataBagDetailsEntityState;

    case DataBagDetailsActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: DataBagDetailsEntityState) => state.entities[id];
