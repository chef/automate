import { mergeMap, catchError, map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of } from 'rxjs';

import {
  CredentialActionTypes,
  SearchCredentials,
  SearchCredentialsSuccess,
  SearchCredentialsFailure,
  CredentialsSearchSuccessPayload
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

  searchCredentials$ = createEffect(() =>
    this.actions$.pipe(
    ofType(CredentialActionTypes.SEARCH),
    mergeMap((action: SearchCredentials) =>
              this.requests.search(action.payload).pipe(
              map(
                (payload: CredentialsSearchSuccessPayload) =>
                  new SearchCredentialsSuccess(payload)),
              catchError((error: HttpErrorResponse) => of(new SearchCredentialsFailure(error)))))));

  searchCredentialsFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(CredentialActionTypes.SEARCH_FAILURE),
    map(({ payload: { error } }: SearchCredentialsFailure) => new CreateNotification({
      type: Type.error,
      message: `Could not get credentials: ${error.error || error}`
    }))));

}
