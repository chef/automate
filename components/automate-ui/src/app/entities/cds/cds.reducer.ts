import { set, pipe } from 'lodash/fp';
import { EntityStatus } from '../entities';
import { CdsActionTypes, CdsActions } from './cds.actions';
import {
  ContentItem
} from './cds.model';

export interface CdsEntityState {
  contentItems: ContentItem[];
  getContentItemsStatus: EntityStatus;
}

export const cdsEntityInitialState: CdsEntityState = {
  contentItems: [],
  getContentItemsStatus: EntityStatus.notLoaded
};

export function desktopEntityReducer(state: CdsEntityState = cdsEntityInitialState,
  action: CdsActions) {

  switch (action.type) {
    case CdsActionTypes.GET_CONTENT_ITEMS:
      return set('getContentItemsStatus', EntityStatus.loading, state);

    case CdsActionTypes.GET_CONTENT_ITEMS_SUCCESS:
      return pipe(
        set('getContentItemsStatus', EntityStatus.loadingSuccess),
        set('contentItems', action.payload))(state);

    case CdsActionTypes.GET_CONTENT_ITEMS_FAILURE:
      return set('getContentItemsStatus', EntityStatus.loadingFailure, state);

    default:
      return state;

  }
}
