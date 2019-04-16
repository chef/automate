import { createSelector, createFeatureSelector } from '@ngrx/store';

import { ServiceGroupEntityState } from './service-groups.reducer';

export const serviceGroupState = createFeatureSelector<ServiceGroupEntityState>('serviceGroups');

export const serviceGroupStatus = createSelector(
  serviceGroupState,
  (state) => state.status
);

export const allServiceGroups = createSelector(
  serviceGroupState,
  (state) => state.serviceGroups
);

export const serviceGroupErrorResp = createSelector(
  serviceGroupState,
  (state) => state.errorResp
);
