import { EntityStatus } from '../entities';
import { ServiceGroupsActionTypes, ServiceGroupsActions } from './service-groups.actions';
import { set, pipe } from 'lodash/fp';
import { ServiceGroup, ServiceGroupFilters } from './service-groups.model';

export interface ServiceGroupEntityState {
  serviceGroups: ServiceGroup[];
  status: EntityStatus;
  filters: ServiceGroupFilters;
}

export const ServiceGroupEntityInitialState: ServiceGroupEntityState = {
  serviceGroups: [],
  status: EntityStatus.notLoaded,
  filters: { }
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
      return set('status', EntityStatus.loadingFailure, state);

    case ServiceGroupsActionTypes.UPDATE_SERVICE_GROUPS_FILTER: {
      const {filters: filters} = action.payload;
      return set('filters', filters)(state);
    }

    default:
      return state;
  }
}
