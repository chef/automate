import { createSelector, createFeatureSelector } from '@ngrx/store';

import {
  AutomateSettingsEntityState
} from './automate-settings.reducer';

export const automateSettingsState =
  createFeatureSelector<AutomateSettingsEntityState>('automateSettings');

export const automateSettingsStatus = createSelector(
  automateSettingsState,
  (state) => state.status
);

export const jobSchedulerStatus = createSelector(
  automateSettingsState,
  (state) => state.jobSchedulerStatus
);

export const changeConfiguration = createSelector(
  automateSettingsState,
  (state) => state.changeConfiguration
);
