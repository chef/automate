import { set, pipe } from 'lodash/fp';
import { EntityStatus } from '../entities';
import { CdsActionTypes, CdsActions } from './cds.actions';
import {
  ContentItem
} from './cds.model';

export interface CdsEntityState {
  contentItems: ContentItem[];
  isContentEnabled: boolean;
  getContentItemsStatus: EntityStatus;
  installContentItemStatus: EntityStatus;
  downloadContentItemStatus: EntityStatus;
  isContentEnabledStatus: EntityStatus;
  submitCredentialsStatus: EntityStatus;
}

export const cdsEntityInitialState: CdsEntityState = {
  contentItems: [],
  isContentEnabled: false,
  getContentItemsStatus: EntityStatus.notLoaded,
  installContentItemStatus: EntityStatus.notLoaded,
  downloadContentItemStatus: EntityStatus.notLoaded,
  isContentEnabledStatus: EntityStatus.notLoaded,
  submitCredentialsStatus: EntityStatus.notLoaded
};

export function desktopEntityReducer(state: CdsEntityState = cdsEntityInitialState,
  action: CdsActions): CdsEntityState {

  switch (action.type) {
    case CdsActionTypes.GET_CONTENT_ITEMS:
      return set('getContentItemsStatus', EntityStatus.loading, state);

    case CdsActionTypes.GET_CONTENT_ITEMS_SUCCESS:
      return pipe(
        set('getContentItemsStatus', EntityStatus.loadingSuccess),
        set('contentItems', action.payload))(state) as CdsEntityState;

    case CdsActionTypes.GET_CONTENT_ITEMS_FAILURE:
      return set('getContentItemsStatus', EntityStatus.loadingFailure, state);

    case CdsActionTypes.INSTALL_CONTENT_ITEM:
      return set('installContentItemStatus', EntityStatus.loading, state);

    case CdsActionTypes.INSTALL_CONTENT_ITEM_SUCCESS:
      return set('installContentItemStatus', EntityStatus.loadingSuccess, state);

    case CdsActionTypes.INSTALL_CONTENT_ITEM_FAILURE:
      return set('installContentItemStatus', EntityStatus.loadingFailure, state);

    case CdsActionTypes.DOWNLOAD_CONTENT_ITEM:
      return set('downloadContentItemStatus', EntityStatus.loading, state);

    case CdsActionTypes.DOWNLOAD_CONTENT_ITEM_SUCCESS:
      return set('downloadContentItemStatus', EntityStatus.loadingSuccess, state);

    case CdsActionTypes.DOWNLOAD_CONTENT_ITEM_FAILURE:
      return set('downloadContentItemStatus', EntityStatus.loadingFailure, state);

    case CdsActionTypes.IS_CONTENT_ENABLED:
      return set('isContentEnabledStatus', EntityStatus.loading, state);

    case CdsActionTypes.IS_CONTENT_ENABLED_SUCCESS:
      return pipe(
        set('isContentEnabledStatus', EntityStatus.loadingSuccess),
        set('isContentEnabled', action.payload))(state) as CdsEntityState;

    case CdsActionTypes.IS_CONTENT_ENABLED_FAILURE:
      return set('isContentEnabledStatus', EntityStatus.loadingFailure, state);

    case CdsActionTypes.SUBMIT_CREDENTIALS:
      return set('submitCredentialsStatus', EntityStatus.loading, state);

    case CdsActionTypes.SUBMIT_CREDENTIALS_SUCCESS:
      return set('submitCredentialsStatus', EntityStatus.loadingSuccess, state);

    case CdsActionTypes.SUBMIT_CREDENTIALS_FAILURE:
      return set('submitCredentialsStatus', EntityStatus.loadingFailure, state);

    default:
      return state;

  }
}
