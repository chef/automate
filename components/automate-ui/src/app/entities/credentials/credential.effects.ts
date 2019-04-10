import { mergeMap, catchError, map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of } from 'rxjs';

import {
  CredentialActionTypes,
  SearchCredentials,
  SearchCredentialsSuccess,
  SearchCredentialsFailure
} from './credential.actions';
import { CredentialRequests } from './credential.requests';
import { CreateNotification } from '../notifications/notification.actions';
import { Type } from '../notifications/notification.model';

@Injectable()
export class CredentialEffects {
  constructor(
    private actions$: Actions,
    private requests: CredentialRequests
  ) {}

  @Effect()
  searchCredentials$ = this.actions$.pipe(
    ofType(CredentialActionTypes.SEARCH),
    mergeMap((action: SearchCredentials) =>
              this.requests.search(action.payload).pipe(
              map(({secrets}) => new SearchCredentialsSuccess(secrets)),
              catchError((error: HttpErrorResponse) => of(new SearchCredentialsFailure(error))))));

  @Effect()
  searchCredentialsFailure$ = this.actions$.pipe(
    ofType(CredentialActionTypes.SEARCH_FAILURE),
    map(({ payload: { error } }: SearchCredentialsFailure) => new CreateNotification({
      type: Type.error,
      message: `Could not get credentials: ${error.error || error}`
    })));
}
