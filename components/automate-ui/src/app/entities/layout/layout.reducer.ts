import { set } from 'lodash/fp';
import { MenuItemGroup } from './layout.model';
import { LayoutActions, LayoutActionTypes } from './layout.actions';

export interface LayoutEntityState {
  menuGroups: MenuItemGroup[];
}

export const InitialState: LayoutEntityState = {
  menuGroups: []
};

export function layoutEntityReducer(
  state: LayoutEntityState = InitialState,
  action: LayoutActions): LayoutEntityState {

  switch (action.type) {

    case LayoutActionTypes.GET_SIDEBAR_MENU_GROUPS:
      return state;

    case LayoutActionTypes.UPDATE_SIDEBAR_MENU_GROUPS: {
      return set('menuGroups', action.payload)(state);
    }

    default:
      return state;

  }
}
