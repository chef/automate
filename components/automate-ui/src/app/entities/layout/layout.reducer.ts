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

// Need to check : why Any for action
export function layoutEntityReducer(
  state: LayoutEntityState = InitialState,
  action: any): LayoutEntityState {

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
