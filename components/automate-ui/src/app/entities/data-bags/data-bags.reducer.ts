import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { HttpErrorResponse } from '@angular/common/http';
import { set, pipe, unset } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { DataBagActionTypes, DataBagActions } from './data-bags.actions';
import { DataBag } from './data-bags.model';

export interface DataBagEntityState extends EntityState<DataBag> {
  getAllStatus: EntityStatus;
  saveStatus:   EntityStatus;
  saveError:    HttpErrorResponse;
  deleteStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';
const SAVE_STATUS    = 'saveStatus';
const SAVE_ERROR     = 'saveError';
const DELETE_STATUS  = 'deleteStatus';

export const dataBagEntityAdapter: EntityAdapter<DataBag> = createEntityAdapter<DataBag>({
  selectId: (dataBag: DataBag) => dataBag.name
});

export const DataBagEntityInitialState: DataBagEntityState =
dataBagEntityAdapter.getInitialState(<DataBagEntityState>{
  getAllStatus: EntityStatus.notLoaded,
  deleteStatus: EntityStatus.notLoaded
});

export function dataBagEntityReducer(
  state: DataBagEntityState = DataBagEntityInitialState,
  action: DataBagActions): DataBagEntityState {

  switch (action.type) {
    case DataBagActionTypes.GET_ALL:
      return set(GET_ALL_STATUS, EntityStatus.loading, dataBagEntityAdapter.removeAll(state));

    case DataBagActionTypes.GET_ALL_SUCCESS:
      return pipe(
        set(GET_ALL_STATUS, EntityStatus.loadingSuccess))
        (dataBagEntityAdapter.setAll(action.payload.data_bags, state)) as DataBagEntityState;

    case DataBagActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    case DataBagActionTypes.CREATE: {
      return set(
        SAVE_STATUS,
        EntityStatus.loading,
        state);
    }

    case DataBagActionTypes.CREATE_SUCCESS: {
      return pipe(
          unset(SAVE_ERROR),
          set(SAVE_STATUS, EntityStatus.loadingSuccess)
        )(dataBagEntityAdapter.addOne(action.payload.databag, state)
      ) as DataBagEntityState;
    }

    case DataBagActionTypes.CREATE_FAILURE: {
      return pipe(
        set(SAVE_ERROR, action.payload),
        set(SAVE_STATUS, EntityStatus.loadingFailure)
      )(state) as DataBagEntityState;
    }

    case DataBagActionTypes.DELETE: {
      return set(DELETE_STATUS, EntityStatus.loading, state);
    }

    case DataBagActionTypes.DELETE_SUCCESS: {
      return set(DELETE_STATUS, EntityStatus.loadingSuccess,
        dataBagEntityAdapter.removeOne(action.payload.name, state));
    }

    case DataBagActionTypes.DELETE_FAILURE: {
      return set(DELETE_STATUS, EntityStatus.loadingFailure, state);
    }

    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: DataBagEntityState) => state.entities[id];
