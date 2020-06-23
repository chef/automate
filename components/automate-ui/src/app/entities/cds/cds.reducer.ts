import { set, pipe } from 'lodash/fp';
import { EntityStatus } from '../entities';
import { CdsActionTypes, CdsActions } from './cds.actions';
import {
  ContentItem
} from './cds.model';

export interface CdsEntityState {
  contentItems: ContentItem[];
  getContentItemsStatus: EntityStatus;
  installContentItemStatus: EntityStatus;
}

export const cdsEntityInitialState: CdsEntityState = {
  contentItems: [],
  getContentItemsStatus: EntityStatus.notLoaded,
  installContentItemStatus: EntityStatus.notLoaded
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

    default:
      return state;

  }
}
