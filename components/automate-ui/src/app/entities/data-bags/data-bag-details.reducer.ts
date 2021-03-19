import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { DataBagItemsActionTypes, DataBagItemsActions } from './data-bag-details.actions';
import { DataBagItems } from './data-bags.model';

export interface DataBagItemsEntityState extends EntityState<DataBagItems> {
  getAllStatus: EntityStatus;
  dataBagItemList: {
    items: DataBagItems[],
    total: number;
  };
  deleteStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';
const DELETE_STATUS  = 'deleteStatus';

export const dataBagItemsEntityAdapter: EntityAdapter<DataBagItems> =
  createEntityAdapter<DataBagItems>({
    selectId: (dataBagItems: DataBagItems) => dataBagItems.name
});

export const DataBagItemsEntityInitialState: DataBagItemsEntityState =
  dataBagItemsEntityAdapter.getInitialState(<DataBagItemsEntityState>{
    getAllStatus: EntityStatus.notLoaded,
    deleteStatus: EntityStatus.notLoaded
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
        set('dataBagItemList.items', action.payload.items || []),
        set('dataBagItemList.total', action.payload.total || 0)
        )(state) as DataBagItemsEntityState;

    case DataBagItemsActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    case DataBagItemsActionTypes.DELETE: {
      return set(DELETE_STATUS, EntityStatus.loading, state);
    }

    case DataBagItemsActionTypes.DELETE_SUCCESS:
      const items =
        state.dataBagItemList.items.filter(item => item.name !== action.payload.name);
      const total = state.dataBagItemList.total - 1;
      return pipe(
        set(DELETE_STATUS, EntityStatus.loadingSuccess),
        set('dataBagItemList.items', items || []),
        set('dataBagItemList.total', total || 0 )
      )(state) as DataBagItemsEntityState;

    case DataBagItemsActionTypes.DELETE_FAILURE: {
      return set(DELETE_STATUS, EntityStatus.loadingFailure, state);
    }

    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: DataBagItemsEntityState) => state.entities[id];
