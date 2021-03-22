import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe, unset } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { DataBagItemsActionTypes, DataBagItemsActions } from './data-bag-details.actions';
import { DataBagItems } from './data-bags.model';
import { HttpErrorResponse } from '@angular/common/http';

export interface DataBagItemsEntityState extends EntityState<DataBagItems> {
  getAllStatus: EntityStatus;
  dataBagItemList: {
    items: DataBagItems[],
    total: number;
  };
  deleteStatus: EntityStatus;
  updateStatus: EntityStatus;
  saveStatus: EntityStatus;
  saveError: HttpErrorResponse;
}

const GET_ALL_STATUS = 'getAllStatus';
const DELETE_STATUS  = 'deleteStatus';
const UPDATE_STATUS  = 'updateStatus';
const SAVE_STATUS = 'saveStatus';
const SAVE_ERROR = 'saveError';


export const dataBagItemsEntityAdapter: EntityAdapter<DataBagItems> =
  createEntityAdapter<DataBagItems>({
    selectId: (dataBagItems: DataBagItems) => dataBagItems.name
});

export const DataBagItemsEntityInitialState: DataBagItemsEntityState =
  dataBagItemsEntityAdapter.getInitialState(<DataBagItemsEntityState>{
    getAllStatus: EntityStatus.notLoaded,
    deleteStatus: EntityStatus.notLoaded,
    updateStatus: EntityStatus.notLoaded
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

    case DataBagItemsActionTypes.CREATE: {
      return set(
        SAVE_STATUS,
        EntityStatus.loading,
        state);
    }

    case DataBagItemsActionTypes.CREATE_SUCCESS: {
      return pipe(
          unset(SAVE_ERROR),
          set(SAVE_STATUS, EntityStatus.loadingSuccess)
        )(dataBagItemsEntityAdapter.addOne(action.payload, state)
      ) as DataBagItemsEntityState;
    }

    case DataBagItemsActionTypes.CREATE_FAILURE: {
      return pipe(
        set(SAVE_ERROR, action.payload),
        set(SAVE_STATUS, EntityStatus.loadingFailure)
      )(state) as DataBagItemsEntityState;
    }

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

    case DataBagItemsActionTypes.UPDATE:
      return set(UPDATE_STATUS, EntityStatus.loading, state);

    case DataBagItemsActionTypes.UPDATE_SUCCESS:
      return set(UPDATE_STATUS, EntityStatus.loadingSuccess,
        dataBagItemsEntityAdapter.updateOne({
          id: action.payload.item_id,
          changes: action.payload
        }, state));

    case DataBagItemsActionTypes.UPDATE_FAILURE:
      return set(UPDATE_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: DataBagItemsEntityState) => state.entities[id];
