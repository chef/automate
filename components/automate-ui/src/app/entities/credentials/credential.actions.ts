import { Action } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';

import { Credential } from './credential.model';

export enum CredentialActionTypes {
  SEARCH         = 'CREDENTIAL::SEARCH',
  SEARCH_SUCCESS = 'CREDENTIAL::SEARCH::SUCCESS',
  SEARCH_FAILURE = 'CREDENTIAL::SEARCH::FAILURE'
}

export class SearchCredentials implements Action {
  readonly type = CredentialActionTypes.SEARCH;
  constructor(public payload: {}) {}
}

export class SearchCredentialsSuccess implements Action {
  readonly type = CredentialActionTypes.SEARCH_SUCCESS;
  constructor(public payload: Credential[]) {}
}

export class SearchCredentialsFailure implements Action {
  readonly type = CredentialActionTypes.SEARCH_FAILURE;
  constructor(public payload: HttpErrorResponse) {}
}

export type CredentialActions =
  | SearchCredentials
  | SearchCredentialsSuccess
  | SearchCredentialsFailure;
