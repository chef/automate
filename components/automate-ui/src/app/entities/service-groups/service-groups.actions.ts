import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { ServiceGroupsPayload } from './service-groups.model';

export enum ServiceGroupsActionTypes {
  GET_SERVICE_GROUPS         = 'SERVICE_GROUPS::GET',
  GET_SERVICE_GROUPS_SUCCESS = 'SERVICE_GROUPS::GET::SUCCESS',
  GET_SERVICE_GROUPS_FAILURE = 'SERVICE_GROUPS::GET::FAILURE',

  UPDATE_SERVICE_GROUPS_FILTER = 'SERVICE_GROUPS::FILTER::UPDATE',
  UPDATE_SERVICE_GROUPS_SORTING = 'SERVICE_GROUPS::SORTING::UPDATE'
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

export type ServiceGroupsActions =
  | GetServiceGroupsSuccess
  | GetServiceGroupsFailure
  | UpdateServiceGroupFilters
  | GetServiceGroups;
