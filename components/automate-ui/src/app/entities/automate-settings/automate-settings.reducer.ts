import { HttpErrorResponse } from '@angular/common/http';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from '../entities';
import { JobSchedulerStatus } from './automate-settings.model';
import { AutomateSettingsActionTypes, AutomateSettingsActions } from './automate-settings.actions';

export interface AutomateSettingsEntityState {
  jobSchedulerStatus: JobSchedulerStatus;
  changeConfiguration: ChangeSettingsStatus;
  status: EntityStatus;
  errorResp: HttpErrorResponse;
}

export interface ChangeSettingsStatus {
  status: EntityStatus;
  errorResp: HttpErrorResponse;
}

export const AutomateSettingsEntityInitialState: AutomateSettingsEntityState = {
  jobSchedulerStatus: null,
  changeConfiguration: {
    status: EntityStatus.notLoaded,
    errorResp: null
  },
  status: EntityStatus.notLoaded,
  errorResp: null
};

export function automateSettingsEntityReducer(
      state: AutomateSettingsEntityState = AutomateSettingsEntityInitialState,
      action: AutomateSettingsActions) {

  switch (action.type) {

    case AutomateSettingsActionTypes.GET_SETTINGS:
      return set('status', EntityStatus.loading, state);

    case AutomateSettingsActionTypes.GET_SETTINGS_SUCCESS:
      return pipe(
        set('status', EntityStatus.loadingSuccess),
        set('jobSchedulerStatus', action.payload.jobSchedulerStatus))(state);

    case AutomateSettingsActionTypes.GET_SETTINGS_FAILURE:
      return pipe(
        set('status', EntityStatus.loadingFailure),
        set('errorResp', action.payload))(state);

    case AutomateSettingsActionTypes.CONFIGURE_SETTINGS:
      return set('changeConfiguration.status', EntityStatus.loading, state);

    case AutomateSettingsActionTypes.CONFIGURE_SETTINGS_SUCCESS:
      return pipe(
        set('changeConfiguration.status', EntityStatus.loadingSuccess),
        set('changeConfiguration.errorResp', null))(state);

    case AutomateSettingsActionTypes.CONFIGURE_SETTINGS_FAILURE:
      return pipe(
        set('changeConfiguration.status', EntityStatus.loadingFailure),
        set('changeConfiguration.errorResp', action.payload))(state);

    default:
      return state;

  }
}

