import { catchError, mergeMap, map, filter } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
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

  getUsers$ = createEffect(() =>
    this.actions$.pipe(ofType<GetUsers>(UserActionTypes.GET_ALL),
    mergeMap((_action: GetUsers) =>
      this.requests.getUsers().pipe(
        map((resp: GetUsersSuccessPayload) => new GetUsersSuccess(resp)),
        catchError((error: HttpErrorResponse) => of(new GetUsersFailure(error))))
    )));

  getUsersFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(UserActionTypes.GET_ALL_FAILURE),
    map(({ payload }: GetUsersFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
        type: Type.error,
        message: `Could not get users: ${msg || payload.error}`
      });
    })));

  getUser$ = createEffect(() =>
    this.actions$.pipe(ofType<GetUser>(UserActionTypes.GET),
    mergeMap((action: GetUser) =>
      this.requests.getUser(action.payload.id).pipe(
        map((resp: User) => new GetUserSuccess(resp)),
        catchError((error: HttpErrorResponse) => of(new GetUserFailure(error))))
    )));

  getUserFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(UserActionTypes.GET_FAILURE),
    map(({ payload: { error } }: GetUserFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get user: ${msg || error}`
      });
    })));

  updatePasswordUser$ = createEffect(() =>
    this.actions$.pipe(
    ofType<UpdatePasswordUser>(UserActionTypes.UPDATE_PASSWORD_USER),
    mergeMap((action: UpdatePasswordUser) =>
      this.requests.updateUser(action.payload).pipe(
        map((resp: User) => new UpdatePasswordUserSuccess(resp)),
        catchError((error: HttpErrorResponse) => of(new UpdateUserFailure(error))))
    )));

  updatePasswordUserSuccess$ = createEffect(() =>
    this.actions$.pipe(
    ofType(UserActionTypes.UPDATE_PASSWORD_USER_SUCCESS),
    map(( { payload: user }: UpdatePasswordUserSuccess) => new CreateNotification({
      type: Type.info,
      message: `Reset password for user: ${user.id}.`
    }))));

  updateUserFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(UserActionTypes.UPDATE_USER_FAILURE),
    map(({ payload: { error } }: UpdateUserFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not update user: ${msg || error}`
      });
    })));

  updateNameUser$ = createEffect(() =>
    this.actions$.pipe(
    ofType<UpdateNameUser>(UserActionTypes.UPDATE_NAME_USER),
    mergeMap((action: UpdateNameUser) =>
      this.requests.updateUser(action.payload).pipe(
        map((resp: User) => new UpdateNameUserSuccess(resp)),
        catchError((error: HttpErrorResponse) => of(new UpdateUserFailure(error))))
    )));

  deleteUser$ = createEffect(() =>
    this.actions$.pipe(ofType<DeleteUser>(UserActionTypes.DELETE),
    mergeMap((action: DeleteUser) =>
      this.requests.deleteUser(action.payload).pipe(
        map((user: User) => new DeleteUserSuccess(user)),
        catchError((error: HttpErrorResponse) => of(new DeleteUserFailure(error))))
    )));

  deleteUserSuccess$ = createEffect(() =>
    this.actions$.pipe(
    ofType(UserActionTypes.DELETE_SUCCESS),
    map(() => new CreateNotification({
      type: Type.info,
      message: 'Successfully deleted user'
    }))));

  deleteUserFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(UserActionTypes.DELETE_FAILURE),
    map(({ payload: { error } }: DeleteUserFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not delete user: ${msg || error}`
      });
    })));

  createUser$ = createEffect(() =>
    this.actions$.pipe(ofType<CreateUser>(UserActionTypes.CREATE),
    mergeMap((action: CreateUser) =>
      this.requests.createUser(action.payload).pipe(
        map((resp: User) => new CreateUserSuccess(resp)),
        catchError((error) => of(new CreateUserFailure(error))))
    )));

  createUserSuccess$ = createEffect(() =>
    this.actions$.pipe(
    ofType(UserActionTypes.CREATE_SUCCESS),
    map(( { payload: user }: CreateUserSuccess)  =>
      new CreateNotification({
        type: Type.info,
        message: `Created user ${user.id}.`
      })
    )));

  createUserFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType<CreateUserFailure>(UserActionTypes.CREATE_FAILURE),
    // username conflict and bad password handled in the modal, see user-management.component.ts
    filter(({ payload: { status } }) => {
      return status !== HttpStatus.CONFLICT && status !== HttpStatus.BAD_REQUEST;
    }),
    map(({ payload: { error } }) => new CreateNotification({
      type: Type.error,
      message: `Could not create user: ${error.error || error}.`
    }))));

}
