import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { Store } from '@ngrx/store';
import { of, combineLatest } from 'rxjs';
import { catchError, mergeMap, map, filter } from 'rxjs/operators';
import { identity } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { HttpStatus } from 'app/types/types';
import { iamMajorVersion } from 'app/entities/policies/policy.selectors';
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
  ApiTokenRequests,
  versionizeToken,
  versionizeTokens
} from './api-token.requests';

@Injectable()
export class ApiTokenEffects {
  constructor(
    private actions$: Actions,
    private requests: ApiTokenRequests,
    private store$: Store<NgrxStateAtom>
  ) { }

  @Effect()
  getAllTokens$ = combineLatest(
    this.actions$.pipe(ofType<GetAllTokens>(ApiTokenActionTypes.GET_ALL)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([_action, version]) =>
        this.requests.getAll(version).pipe(
          map(resp => new GetAllTokensSuccess(versionizeTokens(resp))),
          catchError((error: HttpErrorResponse) => of(new GetAllTokensFailure(error))))));

  @Effect()
  getAllTokensFailure$ = this.actions$.pipe(
    ofType<GetAllTokensFailure>(ApiTokenActionTypes.GET_ALL_FAILURE),
    map(({ payload: { error } }) => new CreateNotification({
      type: Type.error,
      message: `Could not get tokens: ${error.error || error}.`
    })));

  @Effect()
  getToken$ = combineLatest(
    this.actions$.pipe(ofType<GetToken>(ApiTokenActionTypes.GET)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([{ payload: { id } }, version]) =>
        this.requests.get(id, version).pipe(
          map(resp => new GetTokenSuccess(versionizeToken(resp))),
          catchError((error: HttpErrorResponse) => of(new GetTokenFailure(error))))));

  @Effect()
  getTokenFailure$ = this.actions$.pipe(
    ofType<GetTokenFailure>(ApiTokenActionTypes.GET_FAILURE),
    map(({ payload: { error } }) => new CreateNotification({
      type: Type.error,
      message: `Could not get token: ${error.error || error}.`
    })));

  @Effect()
  updateToken$ = combineLatest(
    this.actions$.pipe(ofType<UpdateToken>(ApiTokenActionTypes.UPDATE)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([{ payload: { token } }, version]) =>
        this.requests.update(token, version).pipe(
          map(resp => new UpdateTokenSuccess(versionizeToken(resp))),
          catchError((error: HttpErrorResponse) => of(new UpdateTokenFailure(error))))));

  @Effect()
  updateTokenFailure$ = this.actions$.pipe(
    ofType<UpdateTokenFailure>(ApiTokenActionTypes.UPDATE_FAILURE),
    map(({ payload: { error } }) => new CreateNotification({
      type: Type.error,
      message: `Could not update token: ${error.error || error}.`
    })));

  @Effect()
  createToken$ = combineLatest(
    this.actions$.pipe(ofType<CreateToken>(ApiTokenActionTypes.CREATE)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([{ payload: { id, name } }, version]) =>
        this.requests.create(id, name, version).pipe(
          map(resp => new CreateTokenSuccess(versionizeToken(resp))),
          catchError((error: HttpErrorResponse) => of(new CreateTokenFailure(error))))));

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
  toggleToken$ = combineLatest(
    this.actions$.pipe(ofType<ToggleTokenActive>(ApiTokenActionTypes.TOGGLE)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([{ payload: { id, active } }, version]) =>
        this.requests.toggleActive(id, active, version).pipe(
          map(resp => new ToggleTokenActiveSuccess(versionizeToken(resp))),
          catchError((error: HttpErrorResponse) => of(new ToggleTokenActiveFailure(error))))));

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
  deleteToken$ = combineLatest(
    this.actions$.pipe(ofType<DeleteToken>(ApiTokenActionTypes.DELETE)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([{ payload }, version]) =>
        this.requests.delete(payload.id, version).pipe(
          map(() => new DeleteTokenSuccess(payload)),
          catchError((error: HttpErrorResponse) => of(new DeleteTokenFailure(error))))));

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
