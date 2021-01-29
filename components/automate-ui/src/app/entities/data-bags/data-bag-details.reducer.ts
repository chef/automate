import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { DataBagDetailsActionTypes, DataBagDetailsActions } from './data-bag-details.actions';
import { DataBags } from './data-bags.model';

export interface DataBagDetailsEntityState extends EntityState<DataBags> {
  getAllStatus: EntityStatus;
  getSearchStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';
const GET_SEARCH_STATUS = 'getSearchStatus';


export const dataBagDetailsEntityAdapter: EntityAdapter<DataBags> = createEntityAdapter<DataBags>({
  selectId: (dataBags: DataBags) => dataBags.name
});

export const DataBagDetailsEntityInitialState: DataBagDetailsEntityState =
  dataBagDetailsEntityAdapter.getInitialState(<DataBagDetailsEntityState>{
    getAllStatus: EntityStatus.notLoaded
  });

export function dataBagDetailsEntityReducer(
  state: DataBagDetailsEntityState = DataBagDetailsEntityInitialState,
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

    case DataBagDetailsActionTypes.SEARCH:
      return set(GET_SEARCH_STATUS, EntityStatus.loading, dataBagDetailsEntityAdapter
        .removeAll(state));

    case DataBagDetailsActionTypes.SEARCH_SUCCESS:
      return pipe(
        set(GET_SEARCH_STATUS, EntityStatus.loadingSuccess))
        (dataBagDetailsEntityAdapter
          .setAll(action.payload.nodes, state)) as DataBagDetailsEntityState;

    case DataBagDetailsActionTypes.SEARCH_FAILURE:
      return set(GET_SEARCH_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: DataBagDetailsEntityState) => state.entities[id];
