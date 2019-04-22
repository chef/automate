import { HttpErrorResponse } from '@angular/common/http';
import { EntityStatus } from '../entities';
import { ServiceGroupsActionTypes, ServiceGroupsActions } from './service-groups.actions';
import { set, pipe } from 'lodash/fp';
import {
  ServiceGroup, ServiceGroupFilters, Service, ServicesFilters
} from './service-groups.model';

export interface ServiceGroupEntityState {
  serviceGroups: ServiceGroup[];
  status: EntityStatus;
  filters: ServiceGroupFilters;
  servicesStatus: EntityStatus;
  errorResp: HttpErrorResponse;
  servicesFilters: ServicesFilters;
  servicesList: Service[];
  selectedServiceGroupName: string;
}

export const ServiceGroupEntityInitialState: ServiceGroupEntityState = {
  serviceGroups: [],
  status: EntityStatus.notLoaded,
  filters: { },
  servicesStatus: EntityStatus.notLoaded,
  errorResp: null,
  servicesFilters: { },
  servicesList: [],
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

    case ServiceGroupsActionTypes.UPDATE_SELECTED_SERVICE_GROUP:
      return set('servicesFilters', action.payload, state);

    case ServiceGroupsActionTypes.GET_SERVICES_BY_SERVICE_GROUP:
      return set('serviceStatus', EntityStatus.loading, state);

    case ServiceGroupsActionTypes.GET_SERVICES_BY_SERVICE_GROUP_SUCCESS:
      return pipe(
        set('selectedServiceGroupName', action.payload.group),
        set('serviceStatus', EntityStatus.loadingSuccess),
        set('servicesList', action.payload.services))(state);

    case ServiceGroupsActionTypes.GET_SERVICES_BY_SERVICE_GROUP_FAILURE:
      return pipe(
        set('serviceStatus', EntityStatus.loadingFailure),
        set('errorResp', action.payload))(state);

    default:
      return state;
  }
}
