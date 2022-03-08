import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { HttpErrorResponse } from '@angular/common/http';
import { set, pipe, unset } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { OrgActionTypes, OrgActions, UploadSuccessPayload, PreviewSuccessPayload } from './org.actions';
import { Org } from './org.model';

export interface OrgEntityState extends EntityState<Org> {
  getAllStatus: EntityStatus;
  getStatus: EntityStatus;
  createStatus: EntityStatus;
  createError: HttpErrorResponse;
  updateStatus: EntityStatus;
  deleteStatus: EntityStatus;
  uploadStatus: EntityStatus;
  uploadDetails: UploadSuccessPayload;
  cancelStatus: EntityStatus;
  previewStatus: EntityStatus;
  previewData: PreviewSuccessPayload;
  confirmPreviewStatus: EntityStatus;
  checkUserStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';
const GET_STATUS = 'getStatus';
const CREATE_STATUS = 'createStatus';
const CREATE_ERROR = 'createError';
const DELETE_STATUS = 'deleteStatus';
const UPDATE_STATUS = 'updateStatus';
const UPLOAD_STATUS = 'uploadStatus';
const CANCEL_STATUS = 'cancelStatus';
const PREVIEW_STATUS = 'previewStatus';
const CONFIRM_PREVIEW_STATUS = 'confirmPreviewStatus';
const CHECK_USER_STATUS = 'checkUserStatus';

export const orgEntityAdapter: EntityAdapter<Org> = createEntityAdapter<Org>();

export const OrgEntityInitialState: OrgEntityState =
  orgEntityAdapter.getInitialState(<OrgEntityState>{
    getAllStatus: EntityStatus.notLoaded,
    getStatus: EntityStatus.notLoaded,
    createStatus: EntityStatus.notLoaded,
    createError: null,
    deleteStatus: EntityStatus.notLoaded,
    updateStatus: EntityStatus.notLoaded,
    uploadStatus: EntityStatus.notLoaded,
    uploadDetails: null,
    cancelStatus: EntityStatus.notLoaded,
    previewData: null,
    confirmPreviewStatus: EntityStatus.notLoaded,
    checkUserStatus: EntityStatus.notLoaded
  });

export function orgEntityReducer(
  state: OrgEntityState = OrgEntityInitialState,
  action: OrgActions): OrgEntityState {

  switch (action.type) {
    case OrgActionTypes.GET_ALL:
      return set(GET_ALL_STATUS, EntityStatus.loading, state);

    case OrgActionTypes.GET_ALL_SUCCESS:
      return pipe(
        set(GET_ALL_STATUS, EntityStatus.loadingSuccess))
        (orgEntityAdapter.setAll(action.payload.orgs, state)) as OrgEntityState;

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

    case OrgActionTypes.UPLOAD:
      return set(UPLOAD_STATUS, EntityStatus.loading, state);

    case OrgActionTypes.UPLOAD_SUCCESS:
      return pipe(
        set(UPLOAD_STATUS, EntityStatus.loadingSuccess),
        set('uploadDetails', action.payload || [])
      )(state) as OrgEntityState;

    case OrgActionTypes.UPLOAD_FAILURE:
      return set(UPLOAD_STATUS, EntityStatus.loadingFailure, state);

    case OrgActionTypes.CANCEL_MIGRATION:
      return set(CANCEL_STATUS, EntityStatus.loading, state);

    case OrgActionTypes.CANCEL_MIGRATION_SUCCESS:
      return set(CANCEL_STATUS, EntityStatus.loadingSuccess, state);

    case OrgActionTypes.CANCEL_MIGRATION_FAILURE:
      return set(CANCEL_STATUS, EntityStatus.loadingFailure, state);

    case OrgActionTypes.GET_PREVIEW_DATA:
      return set(PREVIEW_STATUS, EntityStatus.loading, state);

    case OrgActionTypes.GET_PREVIEW_DATA_SUCCESS:
      return pipe(
        set('previewData', action.payload),
        set(PREVIEW_STATUS, EntityStatus.loadingSuccess)
      )(state) as OrgEntityState;

    case OrgActionTypes.GET_PREVIEW_DATA_FAILURE:
      return set(PREVIEW_STATUS, EntityStatus.loadingFailure, state);

    case OrgActionTypes.CONFIRM_PREVIEW:
      return set(CONFIRM_PREVIEW_STATUS, EntityStatus.loading, state);

    case OrgActionTypes.CONFIRM_PREVIEW_SUCCESS:
      return set(CONFIRM_PREVIEW_STATUS, EntityStatus.loadingSuccess, state);

    case OrgActionTypes.CONFIRM_PREVIEW_FAILURE:
      return set(CONFIRM_PREVIEW_STATUS, EntityStatus.loadingFailure, state);
      
    case OrgActionTypes.CHECK_USER:
      return set(CHECK_USER_STATUS,EntityStatus.loading ,state);

    case OrgActionTypes.CHECK_USER_SUCCESS:
        return set(CHECK_USER_STATUS,EntityStatus.loadingSuccess ,state);

    case OrgActionTypes.CHECK_USER_FAILURE:
      return set(CHECK_USER_STATUS,EntityStatus.loadingFailure ,state);

    default:
      return state;
  }
}
