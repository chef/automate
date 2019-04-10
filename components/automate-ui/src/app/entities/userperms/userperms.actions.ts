import { Action } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';

export enum UserPermsTypes {
  GET_ALL = 'PERMS::GET_ALL',
  GET_ALL_SUCCESS = 'PERMS::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'PERMS::GET_ALL::FAILURE',
  GET_SOME = 'PERMS::GET_SOME',
  GET_SOME_SUCCESS = 'PERMS::GET_SOME::SUCCESS',
  GET_SOME_FAILURE = 'PERMS::GET_SOME::FAILURE',
  GET_PARAMETERIZED = 'PERMS::GET_PARAM',
  GET_PARAMETERIZED_SUCCESS = 'PERMS::GET_PARAM::SUCCESS',
  GET_PARAMETERIZED_FAILURE = 'PERMS::GET_PARAM::FAILURE'
}

export interface Pair {
  path: string;
  method: string;
}

export interface UserPermsPayload {
  paths: string[];
}

export interface UserPermsParameterizedPayload {
  path: string;
  parameters: string[];
}

// These correspond to automate-gateway/api/authz/response/authz.proto
export interface UserPermsResponsePayload {
  endpoints: EndpointsMap;
}
interface EndpointsMap {
  [path: string]: EndpointStatus;
}

interface EndpointStatus {
  get: boolean;
  put: boolean;
  post: boolean;
  delete: boolean;
  patch: boolean;
}

export class GetAllUserPerms implements Action {
  readonly type = UserPermsTypes.GET_ALL;
}

export class GetAllUserPermsSuccess implements Action {
  readonly type = UserPermsTypes.GET_ALL_SUCCESS;

  constructor(public payload: EndpointsMap) { }
}

export class GetAllUserPermsFailure implements Action {
  readonly type = UserPermsTypes.GET_ALL_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class GetSomeUserPerms implements Action {
  readonly type = UserPermsTypes.GET_SOME;

  constructor(public payload: UserPermsPayload) { }
}

export class GetSomeUserPermsSuccess implements Action {
  readonly type = UserPermsTypes.GET_SOME_SUCCESS;

  constructor(public payload: EndpointsMap) { }
}

export class GetSomeUserPermsFailure implements Action {
  readonly type = UserPermsTypes.GET_SOME_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class GetUserParamPerms implements Action {
  readonly type = UserPermsTypes.GET_PARAMETERIZED;

  constructor(public payload: UserPermsParameterizedPayload) { }
}

export class GetUserParamPermsSuccess implements Action {
  readonly type = UserPermsTypes.GET_PARAMETERIZED_SUCCESS;

  constructor(public payload: EndpointsMap) { }
}

export class GetUserParamPermsFailure implements Action {
  readonly type = UserPermsTypes.GET_PARAMETERIZED_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}
export type UserPermsActions =
  | GetAllUserPerms
  | GetAllUserPermsSuccess
  | GetAllUserPermsFailure
  | GetSomeUserPerms
  | GetSomeUserPermsSuccess
  | GetSomeUserPermsFailure
  | GetUserParamPerms
  | GetUserParamPermsSuccess
  | GetUserParamPermsFailure;
