import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { HttpErrorResponse } from '@angular/common/http';
import { set, pipe, unset } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { OrgActionTypes, OrgActions } from './org.actions';
import { Org } from './org.model';

export interface OrgEntityState extends EntityState<Org> {
  getAllStatus: EntityStatus;
  getStatus: EntityStatus;
  createStatus: EntityStatus;
  createError: HttpErrorResponse;
  updateStatus: EntityStatus;
  deleteStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';
const GET_STATUS = 'getStatus';
const CREATE_STATUS = 'createStatus';
const CREATE_ERROR = 'createError';
const DELETE_STATUS = 'deleteStatus';
const UPDATE_STATUS = 'updateStatus';

export const orgEntityAdapter: EntityAdapter<Org> = createEntityAdapter<Org>();

export const OrgEntityInitialState: OrgEntityState =
  orgEntityAdapter.getInitialState(<OrgEntityState>{
    getAllStatus: EntityStatus.notLoaded,
    getStatus: EntityStatus.notLoaded,
    createStatus: EntityStatus.notLoaded,
    createError: null,
    deleteStatus: EntityStatus.notLoaded,
    updateStatus: EntityStatus.notLoaded
  });

export function orgEntityReducer(
  state: OrgEntityState = OrgEntityInitialState,
  action: OrgActions): OrgEntityState {

  switch (action.type) {
    case OrgActionTypes.GET_ALL:
      return set(GET_ALL_STATUS, EntityStatus.loading, orgEntityAdapter.removeAll(state));

    case OrgActionTypes.GET_ALL_SUCCESS:
      return pipe(
        set(GET_ALL_STATUS, EntityStatus.loadingSuccess))
        (orgEntityAdapter.addAll(action.payload.orgs, state)) as OrgEntityState;

    case OrgActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    case OrgActionTypes.GET:
      return set(GET_STATUS, EntityStatus.loading, orgEntityAdapter.removeAll(state));

    case OrgActionTypes.GET_SUCCESS:
      return set(GET_STATUS, EntityStatus.loadingSuccess,
        orgEntityAdapter.addOne(action.payload.org, state));

    case OrgActionTypes.GET_FAILURE:
      return set(GET_STATUS, EntityStatus.loadingFailure, state);

    case OrgActionTypes.CREATE:
      return set(CREATE_STATUS, EntityStatus.loading, state);

    case OrgActionTypes.CREATE_SUCCESS:
      return pipe(
        set(CREATE_STATUS, EntityStatus.loadingSuccess),
        unset(CREATE_ERROR)
      )(orgEntityAdapter.addOne(action.payload.org, state)) as OrgEntityState;

    case OrgActionTypes.CREATE_FAILURE:
      return pipe(
        set(CREATE_STATUS, EntityStatus.loadingFailure),
        set(CREATE_ERROR, action.payload)
      )(state) as OrgEntityState;

    case OrgActionTypes.DELETE:
      return set(DELETE_STATUS, EntityStatus.loading, state);

    case OrgActionTypes.DELETE_SUCCESS:
      return set(DELETE_STATUS, EntityStatus.loadingSuccess,
        orgEntityAdapter.removeOne(action.payload.id, state));

    case OrgActionTypes.DELETE_FAILURE:
      return set(DELETE_STATUS, EntityStatus.loadingFailure, state);

    case OrgActionTypes.UPDATE:
      return set(UPDATE_STATUS, EntityStatus.loading, state);

    case OrgActionTypes.UPDATE_SUCCESS:
      return set(UPDATE_STATUS, EntityStatus.loadingSuccess,
        orgEntityAdapter.updateOne({
          id: action.payload.org.id,
          changes: action.payload.org
        }, state));

    case OrgActionTypes.UPDATE_FAILURE:
      return set(UPDATE_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}
