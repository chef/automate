import { createFeatureSelector, createSelector } from '@ngrx/store';

import { LicenseStatusEntityState } from './license.reducer';

const NGRX_REDUCERS_KEY = 'licenseStatus';

export const licenseStatusState =
  createFeatureSelector<LicenseStatusEntityState>(NGRX_REDUCERS_KEY);

export const fetchLicense = createSelector(
  licenseStatusState,
  (state) => state.fetch);

export const applyLicense = createSelector(
  licenseStatusState,
  (state) => state.apply);

export const requestLicense = createSelector(
  licenseStatusState,
  (state) => state.request);

export const triggerWelcome = createSelector(
  licenseStatusState,
  (state) => state.triggerWelcome);
