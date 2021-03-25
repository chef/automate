import { catchError, map, mergeMap, filter } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
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

@Injectable()
export class UserSelfEffects {
  constructor(
    private actions$: Actions,
    private requests: UserRequests,
    private store$: Store<NgrxStateAtom>
  ) { }

  getUserSelf$ = createEffect(() =>
    combineLatest([
      this.actions$.pipe(ofType<GetUserSelf>(UserSelfActionTypes.GET)),
      this.store$.select(userSelfId).pipe(filter(identity))]).pipe(
        mergeMap(([_action, userId]) =>
          this.requests.getUser(userId).pipe(
          map((resp: SelfUser) => new GetUserSelfSuccess(resp)),
          catchError((error: HttpErrorResponse) => of(new GetUserSelfFailure(error)))))
        ));

  getUserSelfFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(UserSelfActionTypes.GET_FAILURE),
    // when first logging in, we see this failure with no error.
    // We do not see the error in the network tab.
    // It looks like it is because the request is canceled.
    // Here we will filter out that error.
    filter(({ payload: { error } }: GetUserSelfFailure) => error),
    map(({ payload: { error } }) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get logged in user: ${msg || error}`
      });
    })));

  updatePasswordSelf$ = createEffect(() =>
    this.actions$.pipe(
    ofType<UpdatePasswordSelf>(UserSelfActionTypes.UPDATE_PASSWORD_SELF),
    mergeMap((action: UpdatePasswordSelf) =>
      this.requests.updateSelf(action.payload).pipe(
        map((resp: SelfUser) => new UpdatePasswordSelfSuccess(resp)),
        catchError((error: HttpErrorResponse) => of(new UpdatePasswordSelfFailure(error))))
    )));

  updatePasswordSelfSuccess$ = createEffect(() =>
    this.actions$.pipe(
    ofType(UserSelfActionTypes.UPDATE_PASSWORD_SELF_SUCCESS),
    map(( { payload: user }: UpdatePasswordSelfSuccess) => new CreateNotification({
      type: Type.info,
      message: `Reset password for user: ${user.id}.`
    }))));

  updateUserFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(UserSelfActionTypes.UPDATE_PASSWORD_SELF_FAILURE),
    map(({ payload: { error } }: UpdatePasswordSelfFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not reset password: ${msg || error}`
      });
    })));

  updateNameSelf$ = createEffect(() =>
    this.actions$.pipe(ofType<UpdateNameSelf>(UserSelfActionTypes.UPDATE_NAME_SELF),
    mergeMap((action: UpdateNameSelf) =>
    this.requests.updateSelf(action.payload).pipe(
      map((resp: SelfUser) => new UpdateNameSelfSuccess(resp)),
      catchError((error: HttpErrorResponse) => of(new UpdateNameSelfFailure(error))))
    )));

  // We are intentionally not sending a notification to the banner for
  // UpdateNameSelfSuccess actions. A notification is provided next to the save button
  // on the user details page.
}
