import { Action } from '@ngrx/store';
import { Sidebars } from './layout.model';

export enum LayoutActionTypes {
  SHOW_PAGE_LOADING = 'Layout::SHOW_PAGE_LOADING',
  GET_SIDEBAR = 'LAYOUT::GET_SIDEBAR',
  UPDATE_SIDEBARS = 'LAYOUT::UPDATE_SIDEBARS'
}

export class ShowPageLoading implements Action {
  readonly type = LayoutActionTypes.SHOW_PAGE_LOADING;
  constructor(public payload: boolean) {}
}

export class GetSidebar implements Action {
  readonly type = LayoutActionTypes.GET_SIDEBAR;
  constructor() {}
}

export class UpdateSidebars implements Action {
  readonly type = LayoutActionTypes.UPDATE_SIDEBARS;

  constructor(public payload: Sidebars ) {}
}

export type LayoutActions =
  | ShowPageLoading
  | GetSidebar
  | UpdateSidebars;
