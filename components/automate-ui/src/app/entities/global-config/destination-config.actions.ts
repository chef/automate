import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';

import { GlobalConfig } from './destination-config.model';

export enum DataFeedGlobalConfigActionTypes {
  GLOBAL_CONFIG                  = 'DESTINATION::GLOBAL_CONFIG',
  GLOBAL_CONFIG_SUCCESS          = 'DESTINATION::GLOBAL_CONFIG::SUCCESS',
  GLOBAL_CONFIG_FAILURE          = 'DESTINATION::GLOBAL_CONFIG::FAILURE'
}

export interface  GlobalDataFeedConfigSuccessPayload {
  config: GlobalConfig;
}

export class GlobalDataFeedConfig implements Action {
  readonly type = DataFeedGlobalConfigActionTypes.GLOBAL_CONFIG;

  constructor(public payload) { }
}
export class GlobalDataFeedConfigSuccess implements Action {
  readonly type = DataFeedGlobalConfigActionTypes.GLOBAL_CONFIG_SUCCESS;

  constructor(public payload) { }
}
export class GlobalDataFeedConfigFailure implements Action {
  readonly type = DataFeedGlobalConfigActionTypes.GLOBAL_CONFIG_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}


export type DestinationActions =
  | GlobalDataFeedConfig
  | GlobalDataFeedConfigSuccess
  | GlobalDataFeedConfigFailure;
