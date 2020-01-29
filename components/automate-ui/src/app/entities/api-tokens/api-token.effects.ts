import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of } from 'rxjs';
import { catchError, mergeMap, map, filter } from 'rxjs/operators';

import { HttpStatus } from 'app/types/types';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';
import {
  ApiTokenActionTypes,
  GetAllTokens,
  GetAllTokensSuccess,
  GetAllTokensFailure,
  GetToken,
  GetTokenSuccess,
  GetTokenFailure,
  UpdateToken,
  UpdateTokenSuccess,
  UpdateTokenFailure,
  CreateToken,
  CreateTokenSuccess,
  CreateTokenFailure,
  ToggleTokenActive,
  ToggleTokenActiveSuccess,
  ToggleTokenActiveFailure,
  DeleteToken,
  DeleteTokenSuccess,
  DeleteTokenFailure
} from './api-token.actions';
import {
  ApiTokenRequests
} from './api-token.requests';

@Injectable()
export class ApiTokenEffects {
  constructor(
    private actions$: Actions,
    private requests: ApiTokenRequests
  ) { }

  @Effect()
  getAllTokens$ = this.actions$.pipe(ofType<GetAllTokens>(ApiTokenActionTypes.GET_ALL),
    mergeMap((_action) =>
      this.requests.getAll().pipe(
        map(resp => new GetAllTokensSuccess(resp)),
        catchError((error: HttpErrorResponse) => of(new GetAllTokensFailure(error))))
    ));

  @Effect()
  getAllTokensFailure$ = this.actions$.pipe(
    ofType<GetAllTokensFailure>(ApiTokenActionTypes.GET_ALL_FAILURE),
    map(({ payload: { error } }) => new CreateNotification({
      type: Type.error,
      message: `Could not get tokens: ${error.error || error}.`
    })));

  @Effect()
  getToken$ = this.actions$.pipe(ofType<GetToken>(ApiTokenActionTypes.GET),
    mergeMap(({ payload: { id } }: GetToken) =>
      this.requests.get(id).pipe(
        map(resp => new GetTokenSuccess(resp.token)),
        catchError((error: HttpErrorResponse) => of(new GetTokenFailure(error))))
    ));

  @Effect()
  getTokenFailure$ = this.actions$.pipe(
    ofType<GetTokenFailure>(ApiTokenActionTypes.GET_FAILURE),
    map(({ payload: { error } }) => new CreateNotification({
      type: Type.error,
      message: `Could not get token: ${error.error || error}.`
    })));

  @Effect()
  updateToken$ = this.actions$.pipe(ofType<UpdateToken>(ApiTokenActionTypes.UPDATE),
    mergeMap(({ payload }: UpdateToken) =>
      this.requests.update(payload).pipe(
        map(resp => new UpdateTokenSuccess(resp.token)),
        catchError((error: HttpErrorResponse) => of(new UpdateTokenFailure(error))))
    ));

  @Effect()
  updateTokenFailure$ = this.actions$.pipe(
    ofType<UpdateTokenFailure>(ApiTokenActionTypes.UPDATE_FAILURE),
    map(({ payload: { error } }) => new CreateNotification({
      type: Type.error,
      message: `Could not update token: ${error.error || error}.`
    })));

  @Effect()
  createToken$ = this.actions$.pipe(ofType<CreateToken>(ApiTokenActionTypes.CREATE),
    mergeMap(({ payload }: CreateToken) =>
      this.requests.create(payload).pipe(
        map(resp => new CreateTokenSuccess(resp.token)),
        catchError((error: HttpErrorResponse) => of(new CreateTokenFailure(error))))
    ));

  @Effect()
  createTokenSuccess$ = this.actions$.pipe(
    ofType<CreateTokenSuccess>(ApiTokenActionTypes.CREATE_SUCCESS),
    map(({ payload: { name } }) => new CreateNotification({
      type: Type.info,
      message: `Created token ${name}.`
    })));

  @Effect()
  createTokenFailure$ = this.actions$.pipe(
    ofType<CreateTokenFailure>(ApiTokenActionTypes.CREATE_FAILURE),
    // ID conflict handled in the modal, see api-token-list.component.ts
    filter(({ payload: { status } }) => status !== HttpStatus.CONFLICT),
    map(({ payload: { error } }) => new CreateNotification({
      type: Type.error,
      message: `Could not create token: ${error.error || error}.`
    })));

  @Effect()
  toggleToken$ = this.actions$.pipe(ofType<ToggleTokenActive>(ApiTokenActionTypes.TOGGLE),
    mergeMap(({ payload: { id, active } }: ToggleTokenActive) =>
      this.requests.toggleActive(id, active).pipe(
        map(resp => new ToggleTokenActiveSuccess(resp.token)),
        catchError((error: HttpErrorResponse) => of(new ToggleTokenActiveFailure(error))))
    ));

  @Effect()
  toggleTokenSuccess$ = this.actions$.pipe(
    ofType<ToggleTokenActiveSuccess>(ApiTokenActionTypes.TOGGLE_SUCCESS),
    map(({ payload: { name, active } }) => new CreateNotification({
      type: Type.info,
      message: `Set token ${name} to ${active ? 'active' : 'inactive'}.`
    })));

  @Effect()
  toggleTokenFailure$ = this.actions$.pipe(
    ofType<ToggleTokenActiveFailure>(ApiTokenActionTypes.TOGGLE_FAILURE),
    map(({ payload: { error } }) => new CreateNotification({
      type: Type.error,
      message: `Could not toggle token state: ${error.error || error}.`
    })));

  @Effect()
  deleteToken$ = this.actions$.pipe(ofType<DeleteToken>(ApiTokenActionTypes.DELETE),
    mergeMap(({ payload }: DeleteToken) =>
      this.requests.delete(payload.id).pipe(
        map(() => new DeleteTokenSuccess(payload)),
        catchError((error: HttpErrorResponse) => of(new DeleteTokenFailure(error))))
    ));

  @Effect()
  deleteTokenSuccess$ = this.actions$.pipe(
    ofType<DeleteTokenSuccess>(ApiTokenActionTypes.DELETE_SUCCESS),
    map(({ payload: { name } }) => new CreateNotification({
      type: Type.info,
      message: `Deleted token ${name}.`
    })));

  @Effect()
  deleteTokenFailure$ = this.actions$.pipe(
    ofType<DeleteTokenFailure>(ApiTokenActionTypes.DELETE_FAILURE),
    map(({ payload: { error } }: DeleteTokenFailure) => new CreateNotification({
      type: Type.error,
      message: `Could not delete token: ${error.error || error}.`
    })));
}
