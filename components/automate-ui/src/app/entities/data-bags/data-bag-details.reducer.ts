import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { DataBagItemsActionTypes, DataBagItemsActions } from './data-bag-details.actions';
import { DataBagItems } from './data-bags.model';

export interface DataBagItemsEntityState extends EntityState<DataBagItems> {
  getAllStatus: EntityStatus;
  dataBagItems: {
    items: DataBagItems[],
    total: number;
  };
}

const GET_ALL_STATUS = 'getAllStatus';

export const dataBagItemsEntityAdapter: EntityAdapter<DataBagItems> =
  createEntityAdapter<DataBagItems>({
    selectId: (dataBagItems: DataBagItems) => dataBagItems.name
});

export const DataBagItemsEntityInitialState: DataBagItemsEntityState =
  dataBagItemsEntityAdapter.getInitialState(<DataBagItemsEntityState>{
    getAllStatus: EntityStatus.notLoaded
  });

export function dataBagItemsEntityReducer(
  state: DataBagItemsEntityState = DataBagItemsEntityInitialState,
  action: DataBagItemsActions): DataBagItemsEntityState {

  switch (action.type) {
    case DataBagItemsActionTypes.GET_ALL:
      return set(GET_ALL_STATUS, EntityStatus.loading, dataBagItemsEntityAdapter
        .removeAll(state));

    case DataBagItemsActionTypes.GET_ALL_SUCCESS:
      return pipe(
        set(GET_ALL_STATUS, EntityStatus.loadingSuccess),
        set('dataBagItems.items', action.payload.items || []),
        set('dataBagItems.total', action.payload.total || 0)
        )(state) as DataBagItemsEntityState;

    case DataBagItemsActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: DataBagItemsEntityState) => state.entities[id];
