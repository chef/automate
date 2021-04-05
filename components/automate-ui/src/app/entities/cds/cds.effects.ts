import { catchError, map, mergeMap } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of } from 'rxjs';

import {
  GetContentItemsSuccess,
  GetContentItemsFailure,
  CdsActionTypes,
  InstallContentItem,
  InstallContentItemSuccess,
  InstallContentItemFailure,
  DownloadContentItem,
  DownloadContentItemSuccess,
  DownloadContentItemFailure,
  IsContentEnabled,
  IsContentEnabledFailure,
  IsContentEnabledSuccess,
  SubmitCredentials,
  SubmitCredentialsFailure,
  SubmitCredentialsSuccess
} from './cds.actions';
import { CdsRequests } from './cds.requests';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';
import { saveAs } from 'file-saver';

@Injectable()
export class CdsEffects {
  constructor(
    private actions$: Actions,
    private requests: CdsRequests
  ) { }


  getContentItems$ = createEffect(() =>
    this.actions$.pipe(
    ofType(CdsActionTypes.GET_CONTENT_ITEMS),
    mergeMap( (_action) =>
      this.requests.getContentItems().pipe(
        map(contentItems => new GetContentItemsSuccess( contentItems )),
        catchError((error) => of(new GetContentItemsFailure(error))))
    )));

  getContentItemsFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(CdsActionTypes.GET_CONTENT_ITEMS_FAILURE),
    map(({ payload: { error } }: GetContentItemsFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get content items errors: ${msg || error}`
      });
    })));

  installContentItem$ = createEffect(() =>
    this.actions$.pipe(
    ofType(CdsActionTypes.INSTALL_CONTENT_ITEM),
    mergeMap( (action: InstallContentItem) =>
      this.requests.installContentItem(action.payload.id, action.payload.user).pipe(
        map( _ => new InstallContentItemSuccess( )),
        catchError((error) => of(new InstallContentItemFailure(error))))
    )));

  installContentItemFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(CdsActionTypes.INSTALL_CONTENT_ITEM_FAILURE),
    map(({ payload: { error } }: InstallContentItemFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Error installing content item errors: ${msg || error}`
      });
    })));

  installContentItemSuccess$ = createEffect(() =>
    this.actions$.pipe(
    ofType(CdsActionTypes.INSTALL_CONTENT_ITEM_SUCCESS),
    map((_action: InstallContentItemSuccess) => {
      return new CreateNotification({
        type: Type.info,
        message: 'Content Item was installed'
      });
    })));

  downloadContentItem$ = createEffect(() =>
    this.actions$.pipe(
    ofType(CdsActionTypes.DOWNLOAD_CONTENT_ITEM),
    mergeMap( (action: DownloadContentItem) =>
      this.requests.downloadContentItem(action.payload.id).pipe(
        map( (file: Blob) => {
          saveAs(file, action.payload.filename);
          return new DownloadContentItemSuccess( { name: action.payload.name } );
        }),
        catchError((error) => of(new DownloadContentItemFailure(
          { name: action.payload.name, httpErrorResponse: error})))
    ))));

  downloadContentItemFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(CdsActionTypes.DOWNLOAD_CONTENT_ITEM_FAILURE),
    map(( action: DownloadContentItemFailure) => {
      const error = action.payload.httpErrorResponse.error;
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Error downloading content item "${action.payload.name}" errors: ${msg || error}`
      });
    })));

  downloadContentItemSuccess$ = createEffect(() =>
    this.actions$.pipe(
    ofType(CdsActionTypes.DOWNLOAD_CONTENT_ITEM_SUCCESS),
    map((action: DownloadContentItemSuccess) => {
      return new CreateNotification({
        type: Type.info,
        message: `Content Item "${action.payload.name}" was downloaded`
      });
    })));

  isContentEnabled$ = createEffect(() =>
    this.actions$.pipe(
    ofType(CdsActionTypes.IS_CONTENT_ENABLED),
    mergeMap( (_action) =>
      this.requests.isContentEnabled().pipe(
        map(isContentEnabled => new IsContentEnabledSuccess( isContentEnabled )),
        catchError((error) => of(new IsContentEnabledFailure(error))))
    )));

  isContentEnabledFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(CdsActionTypes.IS_CONTENT_ENABLED_FAILURE),
    map(({ payload: { error } }: IsContentEnabledFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not check if content is enabled: ${msg || error}`
      });
    })));

  submitCredentials$ = createEffect(() =>
    this.actions$.pipe(
    ofType(CdsActionTypes.SUBMIT_CREDENTIALS),
    mergeMap( (action: SubmitCredentials) =>
      this.requests.submitCredentials(action.payload.credentials).pipe(
        map(_ => new SubmitCredentialsSuccess( )),
        catchError((error) => of(new SubmitCredentialsFailure(error))))
    )));

  submitCredentialsSuccess$ = createEffect(() =>
    this.actions$.pipe(
    ofType(CdsActionTypes.SUBMIT_CREDENTIALS_SUCCESS),
    map( (_action) => new IsContentEnabled())));

  submitCredentialsFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(CdsActionTypes.SUBMIT_CREDENTIALS_FAILURE),
    map(({ payload: { error } }: SubmitCredentialsFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Failed to submit credentials: ${msg || error}`
      });
    })));

}
