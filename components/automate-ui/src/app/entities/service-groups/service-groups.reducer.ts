import { HttpErrorResponse } from '@angular/common/http';
import { EntityStatus } from '../entities';
import { ServiceGroupsActionTypes, ServiceGroupsActions } from './service-groups.actions';
import { set, pipe } from 'lodash/fp';

import {
  ServiceGroup, ServiceGroupFilters,
  Service, ServicesFilters,
  HealthSummary
} from './service-groups.model';

export interface ServiceGroupEntityState {
  serviceGroups: ServiceGroup[];
  serviceGroupHealthCounts: HealthSummary;
  status: EntityStatus;
  filters: ServiceGroupFilters;
  servicesStatus: EntityStatus;
  errorResp: HttpErrorResponse;
  servicesFilters: ServicesFilters;
  servicesList: Service[];
  servicesHealthSummary: HealthSummary;
  selectedServiceGroupName: string;
}

export const ServiceGroupEntityInitialState: ServiceGroupEntityState = {
  serviceGroups: [],
  serviceGroupHealthCounts: {
    total: 0,
    ok: 0,
    warning: 0,
    critical: 0,
    unknown: 0
  },
  status: EntityStatus.notLoaded,
  filters: { },
  servicesStatus: EntityStatus.notLoaded,
  errorResp: null,
  servicesFilters: {
    page: 1,
    health: 'total'
  },
  servicesList: [],
  servicesHealthSummary: {
    total: 0,
    ok: 0,
    warning: 0,
    critical: 0,
    unknown: 0
  },
  selectedServiceGroupName: undefined
};

export function serviceGroupEntityReducer(
  state: ServiceGroupEntityState = ServiceGroupEntityInitialState,
  action: ServiceGroupsActions) {

  switch (action.type) {

    case ServiceGroupsActionTypes.GET_SERVICE_GROUPS:
      return set('status', EntityStatus.loading, state);

    case ServiceGroupsActionTypes.GET_SERVICE_GROUPS_SUCCESS:
      return pipe(
        set('status', EntityStatus.loadingSuccess),
        set('serviceGroups', action.payload.service_groups))(state);

    case ServiceGroupsActionTypes.GET_SERVICE_GROUPS_FAILURE:
      return pipe(
        set('status', EntityStatus.loadingFailure),
        set('errorResp', action.payload))(state);

    case ServiceGroupsActionTypes.UPDATE_SERVICE_GROUPS_FILTER: {
      const {filters: filters} = action.payload;
      return set('filters', filters)(state);
    }

    case ServiceGroupsActionTypes.GET_SERVICE_GROUPS_COUNTS:
      return set('status', EntityStatus.loading, state);

    case ServiceGroupsActionTypes.GET_SERVICE_GROUPS_COUNTS_SUCCESS:
      return pipe(
        set('status', EntityStatus.loadingSuccess),
        set('serviceGroupHealthCounts', action.payload))(state);

    case ServiceGroupsActionTypes.GET_SERVICE_GROUPS_COUNTS_FAILURE:
      return pipe(
        set('status', EntityStatus.loadingFailure),
        set('errorResp', action.payload))(state);

    case ServiceGroupsActionTypes.UPDATE_SELECTED_SERVICE_GROUP:
      return set('servicesFilters', action.payload, state);

    case ServiceGroupsActionTypes.GET_SERVICES_BY_SERVICE_GROUP:
      return set('servicesStatus', EntityStatus.loading, state);

    case ServiceGroupsActionTypes.GET_SERVICES_BY_SERVICE_GROUP_SUCCESS:
      return pipe(
        set('selectedServiceGroupName', action.payload.group),
        set('servicesStatus', EntityStatus.loadingSuccess),
        set('servicesHealthSummary', action.payload.services_health_counts),
        set('servicesList', action.payload.services))(state);

    case ServiceGroupsActionTypes.GET_SERVICES_BY_SERVICE_GROUP_FAILURE:
      return pipe(
        set('servicesStatus', EntityStatus.loadingFailure),
        set('errorResp', action.payload))(state);

    default:
      return state;
  }
}
