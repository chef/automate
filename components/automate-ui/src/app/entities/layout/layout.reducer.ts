import { set } from 'lodash/fp';
import { Sidebars } from './layout.model';
import { LayoutActions, LayoutActionTypes } from './layout.actions';

export interface LayoutEntityState {
  showPageLoading: boolean;
  sidebars: Sidebars;
}

export const InitialState: LayoutEntityState = {
  showPageLoading: false,
  sidebars: {}
};

export function layoutEntityReducer(
  state: LayoutEntityState = InitialState,
  action: LayoutActions): LayoutEntityState {

  switch (action.type) {

    case LayoutActionTypes.SHOW_PAGE_LOADING: {
      return set('showPageLoading', action.payload)(state);
    }

    case LayoutActionTypes.UPDATE_SIDEBARS: {
      return set('sidebars', action.payload)(state);
    }

    default:
      return state;

  }
}
