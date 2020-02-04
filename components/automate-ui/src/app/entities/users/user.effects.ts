import { catchError, mergeMap, map, filter } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of } from 'rxjs';

import { HttpStatus } from 'app/types/types';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';
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
  GetUser,
  GetUserSuccess,
  GetUserFailure,
  UpdatePasswordUser,
  UpdatePasswordUserSuccess,
  UpdateUserFailure,
  UpdateNameUser,
  UpdateNameUserSuccess,
  UserActionTypes
} from './user.actions';
import { UserRequests } from './user.requests';
import { User } from './user.model';

@Injectable()
export class UserEffects {
  constructor(
    private actions$: Actions,
    private requests: UserRequests
  ) { }

  @Effect()
  getUsers$ = this.actions$.pipe(ofType<GetUsers>(UserActionTypes.GET_ALL),
    mergeMap((_action: GetUsers) =>
      this.requests.getUsers().pipe(
        map((resp: GetUsersSuccessPayload) => new GetUsersSuccess(resp)),
        catchError((error: HttpErrorResponse) => of(new GetUsersFailure(error))))
    ));

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

  @Effect()
  getUser$ = this.actions$.pipe(ofType<GetUser>(UserActionTypes.GET),
    mergeMap((action: GetUser) =>
      this.requests.getUser(action.payload.id).pipe(
        map((resp: User) => new GetUserSuccess(resp)),
        catchError((error: HttpErrorResponse) => of(new GetUserFailure(error))))
    ));

  @Effect()
  getUserFailure$ = this.actions$.pipe(
    ofType(UserActionTypes.GET_FAILURE),
    map(({ payload: { error } }: GetUserFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get user: ${msg || error}`
      });
    }));

  @Effect()
  updatePasswordUser$ = this.actions$.pipe(
    ofType<UpdatePasswordUser>(UserActionTypes.UPDATE_PASSWORD_USER),
    mergeMap((action: UpdatePasswordUser) =>
      this.requests.updateUser(action.payload).pipe(
        map((resp: User) => new UpdatePasswordUserSuccess(resp)),
        catchError((error: HttpErrorResponse) => of(new UpdateUserFailure(error))))
    ));

  @Effect()
  updatePasswordUserSuccess$ = this.actions$.pipe(
    ofType(UserActionTypes.UPDATE_PASSWORD_USER_SUCCESS),
    map(( { payload: user }: UpdatePasswordUserSuccess) => new CreateNotification({
      type: Type.info,
      message: `Reset password for user: ${user.id}.`
    })));

  @Effect()
  updateUserFailure$ = this.actions$.pipe(
    ofType(UserActionTypes.UPDATE_USER_FAILURE),
    map(({ payload: { error } }: UpdateUserFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not update user: ${msg || error}`
      });
    }));

  @Effect()
  updateNameUser$ = this.actions$.pipe(
    ofType<UpdateNameUser>(UserActionTypes.UPDATE_NAME_USER),
    mergeMap((action: UpdateNameUser) =>
      this.requests.updateUser(action.payload).pipe(
        map((resp: User) => new UpdateNameUserSuccess(resp)),
        catchError((error: HttpErrorResponse) => of(new UpdateUserFailure(error))))
    ));

  @Effect()
  deleteUser$ = this.actions$.pipe(ofType<DeleteUser>(UserActionTypes.DELETE),
    mergeMap((action: DeleteUser) =>
      this.requests.deleteUser(action.payload).pipe(
        map((user: User) => new DeleteUserSuccess(user)),
        catchError((error: HttpErrorResponse) => of(new DeleteUserFailure(error))))
    ));

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
  createUser$ = this.actions$.pipe(ofType<CreateUser>(UserActionTypes.CREATE),
    mergeMap((action: CreateUser) =>
      this.requests.createUser(action.payload).pipe(
        map((resp: User) => new CreateUserSuccess(resp)),
        catchError((error) => of(new CreateUserFailure(error))))
    ));

  @Effect()
  createUserSuccess$ = this.actions$.pipe(
    ofType(UserActionTypes.CREATE_SUCCESS),
    map(( { payload: user }: CreateUserSuccess)  =>
      new CreateNotification({
        type: Type.info,
        message: `Created user: ${user.id}.`
      })
    ));

  @Effect()
  createUserFailure$ = this.actions$.pipe(
    ofType<CreateUserFailure>(UserActionTypes.CREATE_FAILURE),
    // username conflict and bad password handled in the modal, see user-management.component.ts
    filter(({ payload: { status } }) => {
      return status !== HttpStatus.CONFLICT && status !== HttpStatus.BAD_REQUEST;
    }),
    map(({ payload: { error } }) => new CreateNotification({
      type: Type.error,
      message: `Could not create user: ${error.error || error}.`
    })));
}
