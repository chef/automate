import { Action } from '@ngrx/store';
import { MenuItemGroup } from './layout.model';

export enum LayoutActionTypes {
  SHOW_PAGE_LOADING = 'Layout::SHOW_PAGE_LOADING',
  GET_SIDEBAR_MENU_GROUPS = 'LAYOUT::GET',
  UPDATE_SIDEBAR_MENU_GROUPS = 'LAYOUT::UPDATE'
}

export class ShowPageLoading implements Action {
  readonly type = LayoutActionTypes.SHOW_PAGE_LOADING;
  constructor(public payload: boolean) {}
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
  | ShowPageLoading
  | GetSidebarMenuGroups
  | UpdateSidebarMenuGroups;
