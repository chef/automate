import { catchError, mergeMap, map, filter } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Store } from '@ngrx/store';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of, combineLatest } from 'rxjs';
import { identity } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { CreateNotification } from '../notifications/notification.actions';
import { iamMajorVersion } from 'app/entities/policies/policy.selectors';
import { Type } from '../notifications/notification.model';
import {
  CreateUser,
  CreateUserSuccess,
  CreateUserFailure,
  DeleteUser,
  DeleteUserSuccess,
  DeleteUserFailure,
  GetUsers,
  GetUsersSuccess,
  GetUsersSuccessPayload,
  GetUsersFailure,
  GetUserByUsername,
  GetUserByUsernameSuccess,
  GetUserByUsernameFailure,
  UpdateUser,
  UpdateUserSuccess,
  UpdateUserFailure,
  UpdateSelf,
  UpdateSelfSuccess,
  UserActionTypes
} from './user.actions';
import { UserRequests } from './user.requests';
import { User } from './user.model';
import { SelfUser } from './userself.model';

@Injectable()
export class UserEffects {
  constructor(
    private actions$: Actions,
    private requests: UserRequests,
    private store$: Store<NgrxStateAtom>
  ) { }

  // 7/6/18: TODO in follow-up PR: fetch teams for all users - bd
  @Effect()
  getUsers$ = combineLatest(
    this.actions$.pipe(ofType<GetUsers>(UserActionTypes.GET_ALL)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([_action, version]) =>
        this.requests.getUsers(version).pipe(
          map((resp: GetUsersSuccessPayload) => new GetUsersSuccess(resp)),
          catchError((error: HttpErrorResponse) => of(new GetUsersFailure(error))))));

  @Effect()
  getUsersFailure$ = this.actions$.pipe(
    ofType(UserActionTypes.GET_ALL_FAILURE),
    map(({ payload }: GetUsersFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
        type: Type.error,
        message: `Could not get users: ${msg || payload.error}`
      });
    }));

  // TODO rename
  @Effect()
  getUserByUsername$ = combineLatest(
    this.actions$.pipe(ofType<GetUserByUsername>(UserActionTypes.GET_BY_USERNAME)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([action, version]) =>
        this.requests.getUser(action.payload.id, version).pipe(
          map((resp: User) => new GetUserByUsernameSuccess(resp)),
          catchError((error: HttpErrorResponse) => of(new GetUserByUsernameFailure(error))))));

  @Effect()
  getUserByUsernameFailure$ = this.actions$.pipe(
    ofType(UserActionTypes.GET_BY_USERNAME_FAILURE),
    map(({ payload: { error } }: GetUserByUsernameFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get user: ${msg || error}`
      });
    }));

  @Effect()
  updateUser$ = combineLatest(
    this.actions$.pipe(ofType<UpdateUser>(UserActionTypes.UPDATE)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([action, version]) =>
      this.requests.updateUser(action.payload, version).pipe(
        map((resp: User) => new UpdateUserSuccess(resp)),
        catchError((error: HttpErrorResponse) => of(new UpdateUserFailure(error))))));

  @Effect()
  updateUserSuccess$ = this.actions$.pipe(
    ofType(UserActionTypes.UPDATE_SUCCESS),
    map(() => new CreateNotification({
      type: Type.info,
      message: 'Successfully updated user'
    })));

  @Effect()
  updateUserFailure$ = this.actions$.pipe(
    ofType(UserActionTypes.UPDATE_FAILURE),
    map(({ payload: { error } }: UpdateUserFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not update user: ${msg || error}`
      });
    }));

  @Effect()
  updateSelf$ = combineLatest(
    this.actions$.pipe(ofType<UpdateSelf>(UserActionTypes.UPDATE_SELF)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([action, version]) =>
      this.requests.updateSelf(action.payload, version).pipe(
        map((resp: SelfUser) => new UpdateSelfSuccess(resp)),
        catchError((error: HttpErrorResponse) => of(new UpdateUserFailure(error))))));

  @Effect()
  updateSelfSuccess$ = this.actions$.pipe(
    ofType(UserActionTypes.UPDATE_SELF_SUCCESS),
    map(() => new CreateNotification({
      type: Type.info,
      message: 'Successfully updated your details'
    })));

  @Effect()
  deleteUser$ = combineLatest(
    this.actions$.pipe(ofType<DeleteUser>(UserActionTypes.DELETE)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([action, version]) =>
      this.requests.deleteUser(action.payload, version).pipe(
        map((user: User) => new DeleteUserSuccess(user)),
        catchError((error: HttpErrorResponse) => of(new DeleteUserFailure(error))))));

  @Effect()
  deleteUserSuccess$ = this.actions$.pipe(
    ofType(UserActionTypes.DELETE_SUCCESS),
    map(() => new CreateNotification({
      type: Type.info,
      message: 'Successfully deleted user'
    })));

  @Effect()
  deleteUserFailure$ = this.actions$.pipe(
    ofType(UserActionTypes.DELETE_FAILURE),
    map(({ payload: { error } }: DeleteUserFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not delete user: ${msg || error}`
      });
    }));

  @Effect()
  createUser$ = combineLatest(
    this.actions$.pipe(ofType<CreateUser>(UserActionTypes.CREATE)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([action, version]) =>
      this.requests.createUser(action.payload, version).pipe(
        map((resp: User) => new CreateUserSuccess(resp)),
        catchError((error) => of(new CreateUserFailure(error))))));

  @Effect()
  createUserSuccess$ = this.actions$.pipe(
    ofType(UserActionTypes.CREATE_SUCCESS),
    map(() => new CreateNotification({
      type: Type.info,
      message: 'Successfully created a new user'
    })));

  @Effect()
  createUserFailure$ = this.actions$.pipe(
    ofType(UserActionTypes.CREATE_FAILURE),
    map(({ payload }: CreateUserFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not create user: ${msg || payload.error}`
      });
    }));
}
