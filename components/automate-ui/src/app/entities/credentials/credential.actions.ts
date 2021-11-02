import { Action } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';

import { Credential } from './credential.model';

export enum CredentialActionTypes {
  SEARCH         = 'CREDENTIAL::SEARCH',
  SEARCH_SUCCESS = 'CREDENTIAL::SEARCH::SUCCESS',
  SEARCH_FAILURE = 'CREDENTIAL::SEARCH::FAILURE'
}

export interface NodeCredentialsSearchPayload {
  filters?: any[];
  page?: number;
  per_page?: number;
}

export interface CredentialsSearchSuccessPayload {
  secrets: Credential[];
  total?: number;
}

export class SearchCredentials implements Action {
  readonly type = CredentialActionTypes.SEARCH;
  constructor(public payload: NodeCredentialsSearchPayload) {}
}

export class SearchCredentialsSuccess implements Action {
  readonly type = CredentialActionTypes.SEARCH_SUCCESS;
  constructor(public payload: CredentialsSearchSuccessPayload) {}
}

export class SearchCredentialsFailure implements Action {
  readonly type = CredentialActionTypes.SEARCH_FAILURE;
  constructor(public payload: HttpErrorResponse) {}
}

export type CredentialActions =
  | SearchCredentials
  | SearchCredentialsSuccess
  | SearchCredentialsFailure;
