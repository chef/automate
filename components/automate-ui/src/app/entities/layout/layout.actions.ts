import { Action } from '@ngrx/store';
import { MenuItemGroup } from './layout.model';

export enum LayoutActionTypes {
  GET_SIDEBAR_MENU_GROUPS = 'LAYOUT::GET',
  UPDATE_SIDEBAR_MENU_GROUPS = 'LAYOUT::UPDATE'
}

export class GetSidebarMenuGroups implements Action {
  readonly type = LayoutActionTypes.GET_SIDEBAR_MENU_GROUPS;
  constructor() {}
}

export class UpdateSidebarMenuGroups implements Action {
  readonly type = LayoutActionTypes.UPDATE_SIDEBAR_MENU_GROUPS;

  constructor(public payload: MenuItemGroup[] ) {}
}

export type LayoutActions =
  | GetSidebarMenuGroups
  | UpdateSidebarMenuGroups;
