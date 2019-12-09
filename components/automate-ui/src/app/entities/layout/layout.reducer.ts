import { set } from 'lodash/fp';
import { MenuItemGroup } from './layout.model';
import { LayoutActions, LayoutActionTypes } from './layout.actions';

export interface LayoutEntityState {
  showPageLoading: boolean;
  menuGroups: MenuItemGroup[];
}

export const InitialState: LayoutEntityState = {
  showPageLoading: false,
  menuGroups: []
};

export function layoutEntityReducer(
  state: LayoutEntityState = InitialState,
  action: LayoutActions): LayoutEntityState {

  switch (action.type) {

    case LayoutActionTypes.SHOW_PAGE_LOADING: {
      return set('showPageLoading', action.payload)(state);
    }

    case LayoutActionTypes.UPDATE_SIDEBAR_MENU_GROUPS: {
      return set('menuGroups', action.payload)(state);
    }

    default:
      return state;

  }
}
