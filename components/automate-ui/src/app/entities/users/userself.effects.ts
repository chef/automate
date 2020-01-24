import { catchError, map, mergeMap, filter } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of, combineLatest } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Store } from '@ngrx/store';
import { identity } from 'lodash/fp';

import { userSelfId } from './userself.selectors';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';
import {
  UserSelfActionTypes,
  GetUserSelf,
  GetUserSelfSuccess,
  GetUserSelfFailure,
  UpdatePasswordSelf,
  UpdateNameSelf,
  UpdatePasswordSelfSuccess,
  UpdateNameSelfSuccess,
  UpdateNameSelfFailure,
  UpdatePasswordSelfFailure
} from './userself.actions';
import { UserRequests } from './user.requests';
import { SelfUser } from './userself.model';
import { iamMajorVersion } from 'app/entities/policies/policy.selectors';
import { IAMMajorVersion } from 'app/entities/policies/policy.model';

@Injectable()
export class UserSelfEffects {
  constructor(
    private actions$: Actions,
    private requests: UserRequests,
    private store$: Store<NgrxStateAtom>
  ) { }

  @Effect()
  getUserSelf$ = combineLatest([
    this.actions$.pipe(ofType<GetUserSelf>(UserSelfActionTypes.GET)),
    this.store$.select(userSelfId).pipe(filter(identity))]).pipe(
      mergeMap(([_action, userId]: [GetUserSelf, string]) =>
        this.requests.getUser(userId).pipe(
        map((resp: SelfUser) => new GetUserSelfSuccess(resp)),
        catchError((error: HttpErrorResponse) => of(new GetUserSelfFailure(error)))))
      );

  @Effect()
  getUserSelfFailure$ = this.actions$.pipe(
    ofType(UserSelfActionTypes.GET_FAILURE),
    map(({ payload: { error } }: GetUserSelfFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get logged in user: ${msg || error}`
      });
    }));

    @Effect()
    setUserSelfId$ = this.actions$.pipe(
      ofType(UserSelfActionTypes.SET_ID),
      map(() => new GetUserSelf()));

    @Effect()
    updatePasswordSelf$ = combineLatest([
      this.actions$.pipe(ofType<UpdatePasswordSelf>(UserSelfActionTypes.UPDATE_PASSWORD_SELF)),
      this.store$.select(iamMajorVersion).pipe(filter(identity))])
      .pipe(
        mergeMap(([action, version]: [UpdatePasswordSelf, IAMMajorVersion]) =>
        this.requests.updateSelf(action.payload, version).pipe(
          map((resp: SelfUser) => new UpdatePasswordSelfSuccess(resp)),
          catchError((error: HttpErrorResponse) => of(new UpdatePasswordSelfFailure(error))))));

    @Effect()
    updatePasswordSelfSuccess$ = this.actions$.pipe(
      ofType(UserSelfActionTypes.UPDATE_PASSWORD_SELF_SUCCESS),
      map(( { payload: user }: UpdatePasswordSelfSuccess) => new CreateNotification({
        type: Type.info,
        message: `Reset password for user: ${user.id}.`
      })));

    @Effect()
    updateNameSelf$ = combineLatest([
      this.actions$.pipe(ofType<UpdateNameSelf>(UserSelfActionTypes.UPDATE_NAME_SELF)),
      this.store$.select(iamMajorVersion).pipe(filter(identity))])
      .pipe(
        mergeMap(([action, version]: [UpdateNameSelf, IAMMajorVersion]) =>
        this.requests.updateSelf(action.payload, version).pipe(
          map((resp: SelfUser) => new UpdateNameSelfSuccess(resp)),
          catchError((error: HttpErrorResponse) => of(new UpdateNameSelfFailure(error))))));
}
