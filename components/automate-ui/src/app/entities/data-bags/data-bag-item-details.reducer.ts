import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { DataBagItemDetailsActionTypes, DataBagItemDetailsActions } from './data-bag-item-details.actions';
import { DataBagsItemDetails } from './data-bags.model';

export interface DataBagItemDetailsEntityState extends EntityState<DataBagsItemDetails> {
  getStatus: EntityStatus;
}

const GET_STATUS = 'getStatus';

export const dataBagItemDetailsEntityAdapter:
EntityAdapter<DataBagsItemDetails> = createEntityAdapter<DataBagsItemDetails>({
  selectId: (dataBagsItem: DataBagsItemDetails) => dataBagsItem.name
});

export const DataBagItemDetailsEntityInitialState: DataBagItemDetailsEntityState =
dataBagItemDetailsEntityAdapter.getInitialState(<DataBagItemDetailsEntityState>{
    getStatus: EntityStatus.notLoaded
  });

export function dataBagItemDetailsEntityReducer(
  state: DataBagItemDetailsEntityState = DataBagItemDetailsEntityInitialState,
  action: DataBagItemDetailsActions): DataBagItemDetailsEntityState {

  switch (action.type) {
    case DataBagItemDetailsActionTypes.GET:
      return set(GET_STATUS, EntityStatus.loading, dataBagItemDetailsEntityAdapter
        .removeAll(state));

    case DataBagItemDetailsActionTypes.GET_SUCCESS:
      return pipe(
        set(GET_STATUS, EntityStatus.loadingSuccess))
        (dataBagItemDetailsEntityAdapter
          .addOne(action.payload, state)) as DataBagItemDetailsEntityState;

    case DataBagItemDetailsActionTypes.GET_FAILURE:
      return set(GET_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: DataBagItemDetailsEntityState) => state.entities[id];
