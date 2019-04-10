import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import {
  JobSchedulerStatus,
  ConfigureSettingsRequest
} from './automate-settings.model';

export enum AutomateSettingsActionTypes {
  GET_SETTINGS = 'AUTOMATE::SETTINGS::GET',
  GET_SETTINGS_SUCCESS = 'AUTOMATE::SETTINGS::GET::SUCCESS',
  GET_SETTINGS_FAILURE = 'AUTOMATE::SETTINGS::GET::FAILURE',
  CONFIGURE_SETTINGS = 'AUTOMATE::SETTINGS::CONFIGURE',
  CONFIGURE_SETTINGS_SUCCESS = 'AUTOMATE::SETTINGS::CONFIGURE::SUCCESS',
  CONFIGURE_SETTINGS_FAILURE = 'AUTOMATE::SETTINGS::CONFIGURE::FAILURE'
}

export class GetSettings implements Action {
  readonly type = AutomateSettingsActionTypes.GET_SETTINGS;
  constructor(public payload: {}) {}
}

export class GetSettingsSuccess implements Action {
  readonly type = AutomateSettingsActionTypes.GET_SETTINGS_SUCCESS;

  constructor(public payload: { jobSchedulerStatus: JobSchedulerStatus }) {}
}

export class GetSettingsFailure implements Action {
  readonly type = AutomateSettingsActionTypes.GET_SETTINGS_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class ConfigureSettings implements Action {
  readonly type = AutomateSettingsActionTypes.CONFIGURE_SETTINGS;

  constructor(public payload: ConfigureSettingsRequest) {}
}

export class ConfigureSettingsSuccess implements Action {
  readonly type = AutomateSettingsActionTypes.CONFIGURE_SETTINGS_SUCCESS;

  constructor(public payload: {}) {}
}

export class ConfigureSettingsFailure implements Action {
  readonly type = AutomateSettingsActionTypes.CONFIGURE_SETTINGS_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export type AutomateSettingsActions =
  | ConfigureSettingsFailure
  | ConfigureSettingsSuccess
  | ConfigureSettings
  | GetSettingsSuccess
  | GetSettingsFailure
  | GetSettings;

