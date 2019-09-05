import { HttpErrorResponse } from '@angular/common/http';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from '../entities';
import { ServiceGroupsActionTypes, ServiceGroupsActions } from './service-groups.actions';

import {
  ServiceGroup,
  ServiceGroupsFilters,
  ServiceGroupsHealthSummary,
  ServiceGroupsSuggestions,
  SelectedServiceGroup
} from './service-groups.model';

export interface ServiceGroupsEntityState {
  error: HttpErrorResponse;
  filters: ServiceGroupsFilters;
  healthSummary: ServiceGroupsHealthSummary;
  list: ServiceGroup[];
  selectedGroup: SelectedServiceGroup;
  status: EntityStatus;
  suggestions: ServiceGroupsSuggestions;
}

export const ServiceGroupEntityInitialState: ServiceGroupsEntityState = {
  error: null,
  filters: {},
  healthSummary: {
    total: 0,
    ok: 0,
    warning: 0,
    critical: 0,
    unknown: 0
  },
  list: [],
  selectedGroup: {
    name: undefined,
    services: {
      error: null,
      filters: {
        page: 1,
        health: 'total'
      },
      healthSummary: {
        total: 0,
        ok: 0,
        warning: 0,
        critical: 0,
        unknown: 0
      },
      list: [],
      status: EntityStatus.notLoaded
    }
  },
  status: EntityStatus.notLoaded,
  suggestions: {
    values: [],
    status: EntityStatus.notLoaded
  }
};

export function serviceGroupsEntityReducer(
  state: ServiceGroupsEntityState = ServiceGroupEntityInitialState,
  action: ServiceGroupsActions) {

  switch (action.type) {

    case ServiceGroupsActionTypes.GET_SERVICE_GROUPS:
      return set('status', EntityStatus.loading, state);

    case ServiceGroupsActionTypes.GET_SERVICE_GROUPS_SUCCESS:
      return pipe(
        set('status', EntityStatus.loadingSuccess),
        set('list', action.payload.service_groups))(state);

    case ServiceGroupsActionTypes.GET_SERVICE_GROUPS_FAILURE:
      return pipe(
        set('status', EntityStatus.loadingFailure),
        set('error', action.payload))(state);

    case ServiceGroupsActionTypes.UPDATE_SERVICE_GROUPS_FILTER: {
      const {filters: filters} = action.payload;
      return set('filters', filters)(state);
    }

    case ServiceGroupsActionTypes.GET_SERVICE_GROUPS_COUNTS:
      return set('status', EntityStatus.loading, state);

    case ServiceGroupsActionTypes.GET_SERVICE_GROUPS_COUNTS_SUCCESS:
      return pipe(
        set('status', EntityStatus.loadingSuccess),
        set('healthSummary', action.payload))(state);

    case ServiceGroupsActionTypes.GET_SERVICE_GROUPS_COUNTS_FAILURE:
      return pipe(
        set('status', EntityStatus.loadingFailure),
        set('error', action.payload))(state);

    case ServiceGroupsActionTypes.UPDATE_SELECTED_SERVICE_GROUP:
      return set('selectedGroup.services.filters', action.payload, state);

    case ServiceGroupsActionTypes.GET_SERVICES_BY_SERVICE_GROUP:
      return set('selectedGroup.services.status', EntityStatus.loading, state);

    case ServiceGroupsActionTypes.GET_SERVICES_BY_SERVICE_GROUP_SUCCESS:
      return pipe(
        set('selectedGroup.name', action.payload.group),
        set('selectedGroup.services.status', EntityStatus.loadingSuccess),
        set('selectedGroup.services.healthSummary', action.payload.services_health_counts),
        set('selectedGroup.services.list', action.payload.services))(state);

    case ServiceGroupsActionTypes.GET_SERVICES_BY_SERVICE_GROUP_FAILURE:
      return pipe(
        set('selectedGroup.services.status', EntityStatus.loadingFailure),
        set('selectedGroup.services.error', action.payload))(state);

    case ServiceGroupsActionTypes.GET_SERVICE_GROUPS_SUGGESTIONS:
      return pipe(
        set('suggestions.status', EntityStatus.loading),
        set('suggestions.values', []))(state);

    case ServiceGroupsActionTypes.GET_SERVICE_GROUPS_SUGGESTIONS_SUCCESS:
      return pipe(
        set('suggestions.status', EntityStatus.loadingSuccess),
        set('suggestions.values', action.payload.serviceGroupsSuggestions))(state);

    case ServiceGroupsActionTypes.GET_SERVICE_GROUPS_SUGGESTIONS_FAILURE:
      return pipe(
        set('suggestions.values', []),
        set('suggestions.status', EntityStatus.loadingFailure),
        set('error', action.payload))(state);

    default:
      return state;
  }
}
