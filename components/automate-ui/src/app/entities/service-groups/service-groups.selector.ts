import { createSelector, createFeatureSelector } from '@ngrx/store';

import { ServiceGroupEntityState } from './service-groups.reducer';

export const serviceGroupState = createFeatureSelector<ServiceGroupEntityState>('serviceGroups');

export const serviceGroupStatus = createSelector(
  serviceGroupState,
  (state) => state.status
);

export const servicesStatus = createSelector(
  serviceGroupState,
  (state) => state.servicesStatus
);

export const allServiceGroups = createSelector(
  serviceGroupState,
  (state) => state.serviceGroups
);

export const allServiceGroupHealth = createSelector(
  serviceGroupState,
  (state) => state.serviceGroupHealthCounts
);

export const serviceGroupErrorResp = createSelector(
  serviceGroupState,
  (state) => state.errorResp
);

export const servicesErrorResp = createSelector(
  serviceGroupState,
  (state) => state.servicesErrorResp
);
