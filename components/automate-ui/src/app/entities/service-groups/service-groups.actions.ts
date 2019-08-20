import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import {
  ServiceGroupsPayload, ServicesPayload,
  ServicesFilters, HealthSummary
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

  GET_SERVICE_GROUP_SUGGESTIONS = 'GET_SERVICE_GROUP_SUGGESTIONS::SERVICES::GET',
  GET_SERVICE_GROUP_SUGGESTIONS_SUCCESS = 'GET_SERVICE_GROUP_SUGGESTIONS::SERVICES::SUCCESS',
  GET_SERVICE_GROUP_SUGGESTIONS_FAILURE = 'GET_SERVICE_GROUP_SUGGESTIONS:SERVICES::FAILURE'
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

export class UpdateServiceGroupFilters implements Action {
  readonly type = ServiceGroupsActionTypes.UPDATE_SERVICE_GROUPS_FILTER;

  constructor(public payload: {filters} ) {}
}

export class GetServiceGroupsCounts implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICE_GROUPS_COUNTS;
  constructor() {}
}

export class GetServiceGroupsCountsSuccess implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICE_GROUPS_COUNTS_SUCCESS;

  constructor(public payload: HealthSummary) {}
}

export class GetServiceGroupsCountsFailure implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICE_GROUPS_COUNTS_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateSelectedSG implements Action {
  readonly type = ServiceGroupsActionTypes.UPDATE_SELECTED_SERVICE_GROUP;

  constructor(public payload: ServicesFilters) {}
}

export class GetServicesBySG implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICES_BY_SERVICE_GROUP;
  constructor() {}
}

export class GetServicesBySGSuccess implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICES_BY_SERVICE_GROUP_SUCCESS;

  constructor(public payload: ServicesPayload ) {}
}

export class GetServicesBySGFailure implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICES_BY_SERVICE_GROUP_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class GetServiceGroupSuggestions implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICE_GROUP_SUGGESTIONS;
  constructor(public payload: { type: string, text: string }) {}
}

export class GetServiceGroupSuggestionsSuccess implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICE_GROUP_SUGGESTIONS_SUCCESS;

  constructor(public payload: { nodeSuggestions: any[] }) {}
}

export class GetServiceGroupSuggestionsFailure implements Action {
  readonly type = ServiceGroupsActionTypes.GET_SERVICE_GROUP_SUGGESTIONS_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export type ServiceGroupsActions =
  | GetServiceGroupsSuccess
  | GetServiceGroupsFailure
  | UpdateServiceGroupFilters
  | GetServiceGroups
  | GetServiceGroupsCounts
  | GetServiceGroupsCountsSuccess
  | GetServiceGroupsCountsFailure
  | UpdateSelectedSG
  | GetServicesBySG
  | GetServicesBySGSuccess
  | GetServicesBySGFailure
  | GetServiceGroups
  | GetServiceGroupSuggestions
  | GetServiceGroupSuggestionsSuccess
  | GetServiceGroupSuggestionsFailure;
