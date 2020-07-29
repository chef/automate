import { Action } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';

import { ContentItem, Credentials } from './cds.model';

export enum CdsActionTypes {
  GET_CONTENT_ITEMS              = 'CDS::GET::CONTENT_ITEMS',
  GET_CONTENT_ITEMS_SUCCESS      = 'CDS::GET::CONTENT_ITEMS::SUCCESS',
  GET_CONTENT_ITEMS_FAILURE      = 'CDS::GET::CONTENT_ITEMS::FAILURE',
  INSTALL_CONTENT_ITEM           = 'CDS::INSTALL::CONTENT_ITEM',
  INSTALL_CONTENT_ITEM_SUCCESS   = 'CDS::INSTALL::CONTENT_ITEM::SUCCESS',
  INSTALL_CONTENT_ITEM_FAILURE   = 'CDS::INSTALL::CONTENT_ITEM::FAILURE',
  DOWNLOAD_CONTENT_ITEM          = 'CDS::DOWNLOAD::CONTENT_ITEM',
  DOWNLOAD_CONTENT_ITEM_SUCCESS  = 'CDS::DOWNLOAD::CONTENT_ITEM::SUCCESS',
  DOWNLOAD_CONTENT_ITEM_FAILURE  = 'CDS::DOWNLOAD::CONTENT_ITEM::FAILURE',
  IS_CONTENT_ENABLED             = 'CDS::IS_CONTENT_ENABLED',
  IS_CONTENT_ENABLED_SUCCESS     = 'CDS::IS_CONTENT_ENABLED::SUCCESS',
  IS_CONTENT_ENABLED_FAILURE     = 'CDS::IS_CONTENT_ENABLED::FAILURE',
  SUBMIT_CREDENTIALS             = 'CDS::SUBMIT_CREDENTIALS',
  SUBMIT_CREDENTIALS_SUCCESS     = 'CDS::SUBMIT_CREDENTIALS::SUCCESS',
  SUBMIT_CREDENTIALS_FAILURE     = 'CDS::SUBMIT_CREDENTIALS::FAILURE'
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
  constructor( public payload: { id: string, user: string }) { }
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

export class IsContentEnabled implements Action {
  readonly type = CdsActionTypes.IS_CONTENT_ENABLED;
}

export class IsContentEnabledSuccess implements Action {
  readonly type = CdsActionTypes.IS_CONTENT_ENABLED_SUCCESS;
  constructor( public payload: boolean ) { }
}

export class IsContentEnabledFailure implements Action {
  readonly type = CdsActionTypes.IS_CONTENT_ENABLED_FAILURE;
  constructor(public payload: HttpErrorResponse ) { }
}

export class SubmitCredentials implements Action {
  readonly type = CdsActionTypes.SUBMIT_CREDENTIALS;
  constructor( public payload: { credentials: Credentials} ) { }
}

export class SubmitCredentialsSuccess implements Action {
  readonly type = CdsActionTypes.SUBMIT_CREDENTIALS_SUCCESS;
}

export class SubmitCredentialsFailure implements Action {
  readonly type = CdsActionTypes.SUBMIT_CREDENTIALS_FAILURE;
  constructor(public payload: HttpErrorResponse ) { }
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
  | DownloadContentItemFailure
  | IsContentEnabled
  | IsContentEnabledSuccess
  | IsContentEnabledFailure
  | SubmitCredentials
  | SubmitCredentialsSuccess
  | SubmitCredentialsFailure;
