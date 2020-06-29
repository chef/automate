import { Action } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';

import { ContentItem } from './cds.model';

export enum CdsActionTypes {
  GET_CONTENT_ITEMS              = 'CDS::GET::CONTENT_ITEMS',
  GET_CONTENT_ITEMS_SUCCESS      = 'CDS::GET::CONTENT_ITEMS::SUCCESS',
  GET_CONTENT_ITEMS_FAILURE      = 'CDS::GET::CONTENT_ITEMS::FAILURE',
  INSTALL_CONTENT_ITEM           = 'CDS::INSTALL::CONTENT_ITEM',
  INSTALL_CONTENT_ITEM_SUCCESS   = 'CDS::INSTALL::CONTENT_ITEM::SUCCESS',
  INSTALL_CONTENT_ITEM_FAILURE   = 'CDS::INSTALL::CONTENT_ITEM::FAILURE',
  DOWNLOAD_CONTENT_ITEM          = 'CDS::DOWNLOAD::CONTENT_ITEM',
  DOWNLOAD_CONTENT_ITEM_SUCCESS  = 'CDS::DOWNLOAD::CONTENT_ITEM::SUCCESS',
  DOWNLOAD_CONTENT_ITEM_FAILURE  = 'CDS::DOWNLOAD::CONTENT_ITEM::FAILURE'
}

export class GetContentItems implements Action {
  readonly type = CdsActionTypes.GET_CONTENT_ITEMS;
}

export class GetContentItemsSuccess implements Action {
  readonly type = CdsActionTypes.GET_CONTENT_ITEMS_SUCCESS;
  constructor(public payload: ContentItem[]) { }
}

export class GetContentItemsFailure implements Action {
  readonly type = CdsActionTypes.GET_CONTENT_ITEMS_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class InstallContentItem implements Action {
  readonly type = CdsActionTypes.INSTALL_CONTENT_ITEM;
  constructor( public payload: { id: string }) { }
}

export class InstallContentItemSuccess implements Action {
  readonly type = CdsActionTypes.INSTALL_CONTENT_ITEM_SUCCESS;
}

export class InstallContentItemFailure implements Action {
  readonly type = CdsActionTypes.INSTALL_CONTENT_ITEM_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class DownloadContentItem implements Action {
  readonly type = CdsActionTypes.DOWNLOAD_CONTENT_ITEM;
  constructor( public payload: { id: string, filename: string, name: string }) { }
}

export class DownloadContentItemSuccess implements Action {
  readonly type = CdsActionTypes.DOWNLOAD_CONTENT_ITEM_SUCCESS;
  constructor( public payload: { name: string }) { }
}

export class DownloadContentItemFailure implements Action {
  readonly type = CdsActionTypes.DOWNLOAD_CONTENT_ITEM_FAILURE;
  constructor(public payload: {httpErrorResponse: HttpErrorResponse, name: string} ) { }
}

export type CdsActions =
  | GetContentItems
  | GetContentItemsSuccess
  | GetContentItemsFailure
  | InstallContentItem
  | InstallContentItemSuccess
  | InstallContentItemFailure
  | DownloadContentItem
  | DownloadContentItemSuccess
  | DownloadContentItemFailure;
