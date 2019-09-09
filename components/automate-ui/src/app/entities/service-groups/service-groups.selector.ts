import { createSelector, createFeatureSelector } from '@ngrx/store';

import { ServiceGroupsEntityState } from './service-groups.reducer';

export const serviceGroupsState = createFeatureSelector<ServiceGroupsEntityState>('serviceGroups');

export const serviceGroupsError = createSelector(
  serviceGroupsState,
  (serviceGroups) => serviceGroups.error
);

export const serviceGroupsFilters = createSelector(
  serviceGroupsState,
  (serviceGroups) => serviceGroups.filters
);

export const serviceGroupsHealth = createSelector(
  serviceGroupsState,
  (serviceGroups) => serviceGroups.healthSummary
);

export const serviceGroupsList = createSelector(
  serviceGroupsState,
  (serviceGroups) => serviceGroups.list
);

export const serviceGroupsStatus = createSelector(
  serviceGroupsState,
  (serviceGroups) => serviceGroups.status
);

export const serviceGroupsSuggestions = createSelector(
  serviceGroupsState,
  (serviceGroups) => serviceGroups.suggestions
);

export const selectedServiceGroup = createSelector(
  serviceGroupsState,
  (serviceGroups) => serviceGroups.selectedGroup
);


export const selectedServiceGroupName = createSelector(
  serviceGroupsState,
  (serviceGroups) => serviceGroups.selectedGroup.name
);

export const selectedServiceGroupServices = createSelector(
  serviceGroupsState,
  (serviceGroups) => serviceGroups.selectedGroup.services
);

export const selectedServiceGroupList = createSelector(
  serviceGroupsState,
  (serviceGroups) => serviceGroups.selectedGroup.services.list
);

export const selectedServiceGroupError = createSelector(
  serviceGroupsState,
  (serviceGroups) => serviceGroups.selectedGroup.services.error
);

export const selectedServiceGroupFilters = createSelector(
  serviceGroupsState,
  (serviceGroups) => serviceGroups.selectedGroup.services.filters
);

export const selectedServiceGroupHealth = createSelector(
  serviceGroupsState,
  (serviceGroups) => serviceGroups.selectedGroup.services.healthSummary
);

export const selectedServiceGroupStatus = createSelector(
  serviceGroupsState,
  (serviceGroups) => serviceGroups.selectedGroup.services.status
);
