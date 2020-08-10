import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import {
  ServiceGroupsHealthSummary,
  ServiceGroupsPayload,
  GroupServicesPayload,
  GroupServicesFilters
} from './service-groups.model';

export enum ServiceGroupsActionTypes {
  GET_SERVICE_GROUPS         = 'SERVICE_GROUPS::GET',
  GET_SERVICE_GROUPS_SUCCESS = 'SERVICE_GROUPS::GET::SUCCESS',
  GET_SERVICE_GROUPS_FAILURE = 'SERVICE_GROUPS::GET::FAILURE',

  GET_SERVICE_GROUPS_COUNTS  = 'SERVICE_GROUPS_COUNTS::GET',
  GET_SERVICE_GROUPS_COUNTS_SUCCESS  = 'SERVICE_GROUPS_COUNTS::GET::SUCCESS',
  GET_SERVICE_GROUPS_COUNTS_FAILURE  = 'SERVICE_GROUPS_COUNTS::GET::FAILURE',

  UPDATE_SERVICE_GROUPS_FILTER = 'SERVICE_GROUPS::FILTER::UPDATE',
  UPDATE_SERVICE_GROUPS_SORTING = 'SERVICE_GROUPS::SORTING::UPDATE',

  UPDATE_SELECTED_SERVICE_GROUP = 'SERVICE_GROUPS::SELECTED::SG::UPDATE',

  GET_SERVICES_BY_SERVICE_GROUP = 'SERVICE_GROUPS::SERVICES::GET',
  GET_SERVICES_BY_SERVICE_GROUP_SUCCESS = 'SERVICE_GROUPS::SERVICES::GET::SUCCESS',
  GET_SERVICES_BY_SERVICE_GROUP_FAILURE = 'SERVICE_GROUPS::SERVICES::GET::FAILURE',

  GET_SERVICE_GROUPS_SUGGESTIONS = 'SERVICE_GROUPS_SUGGESTIONS::SERVICES::GET',
  GET_SERVICE_GROUPS_SUGGESTIONS_SUCCESS = 'SERVICE_GROUPS_SUGGESTIONS::SERVICES::SUCCESS',
  GET_SERVICE_GROUPS_SUGGESTIONS_FAILURE = 'SERVICE_GROUPS_SUGGESTIONS:SERVICES::FAILURE',

  DELETE_SERVICES_BY_ID = 'SERVICE_GROUPS::SERVICES::DELETE',
  DELETE_SERVICES_BY_ID_SUCCESS = 'SERVICE_GROUPS::SERVICES::DELETE::SUCCESS',
  DELETE_SERVICES_BY_ID_FAILURE = 'SERVICE_GROUPS::SERVICES::DELETE::FAILURE'
}

export class GetServiceGroups implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICE_GROUPS;
  constructor() {}
}

export class GetServiceGroupsSuccess implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICE_GROUPS_SUCCESS;

  constructor(public payload: ServiceGroupsPayload) {}
}

export class GetServiceGroupsFailure implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICE_GROUPS_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateServiceGroupsFilters implements Action {
  readonly type = ServiceGroupsActionTypes.UPDATE_SERVICE_GROUPS_FILTER;

  constructor(public payload: {filters} ) {}
}

export class GetServiceGroupsCounts implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICE_GROUPS_COUNTS;
  constructor() {}
}

export class GetServiceGroupsCountsSuccess implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICE_GROUPS_COUNTS_SUCCESS;

  constructor(public payload: ServiceGroupsHealthSummary) {}
}

export class GetServiceGroupsCountsFailure implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICE_GROUPS_COUNTS_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateSelectedSG implements Action {
  readonly type = ServiceGroupsActionTypes.UPDATE_SELECTED_SERVICE_GROUP;

  constructor(public payload: GroupServicesFilters) {}
}

export class GetServicesBySG implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICES_BY_SERVICE_GROUP;
  constructor() {}
}

export class GetServicesBySGSuccess implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICES_BY_SERVICE_GROUP_SUCCESS;

  constructor(public payload: GroupServicesPayload ) {}
}

export class GetServicesBySGFailure implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICES_BY_SERVICE_GROUP_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class GetServiceGroupsSuggestions implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICE_GROUPS_SUGGESTIONS;
  constructor(public payload: { type: string, text: string }) {}
}

export class GetServiceGroupsSuggestionsSuccess implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICE_GROUPS_SUGGESTIONS_SUCCESS;

  constructor(public payload: { serviceGroupsSuggestions: any[] }) {}
}

export class GetServiceGroupsSuggestionsFailure implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICE_GROUPS_SUGGESTIONS_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class DeleteServicesById implements Action {
  readonly type = ServiceGroupsActionTypes.DELETE_SERVICES_BY_ID;
  constructor(public payload: { servicesToDelete: number[] }) {}
}

export class DeleteServicesByIdSuccess implements Action {
  readonly type = ServiceGroupsActionTypes.DELETE_SERVICES_BY_ID_SUCCESS;
  constructor(public payload: { amount: number } ) { }
}

export class DeleteServicesByIdFailure implements Action {
  readonly type = ServiceGroupsActionTypes.DELETE_SERVICES_BY_ID_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type ServiceGroupsActions =
  | GetServiceGroupsSuccess
  | GetServiceGroupsFailure
  | UpdateServiceGroupsFilters
  | GetServiceGroups
  | GetServiceGroupsCounts
  | GetServiceGroupsCountsSuccess
  | GetServiceGroupsCountsFailure
  | UpdateSelectedSG
  | GetServicesBySG
  | GetServicesBySGSuccess
  | GetServicesBySGFailure
  | GetServiceGroups
  | GetServiceGroupsSuggestions
  | GetServiceGroupsSuggestionsSuccess
  | GetServiceGroupsSuggestionsFailure
  | DeleteServicesById
  | DeleteServicesByIdSuccess
  | DeleteServicesByIdFailure;
