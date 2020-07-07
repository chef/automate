import { catchError, map, mergeMap } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Actions, Effect, ofType } from '@ngrx/effects';
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
  IsContentEnabledFailure,
  IsContentEnabledSuccess
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

  @Effect()
  getContentItems$ = this.actions$.pipe(
    ofType(CdsActionTypes.GET_CONTENT_ITEMS),
    mergeMap( (_action) =>
      this.requests.getContentItems().pipe(
        map(contentItems => new GetContentItemsSuccess( contentItems )),
        catchError((error) => of(new GetContentItemsFailure(error))))
    ));

  @Effect()
  getContentItemsFailure$ = this.actions$.pipe(
    ofType(CdsActionTypes.GET_CONTENT_ITEMS_FAILURE),
    map(({ payload: { error } }: GetContentItemsFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get content items errors: ${msg || error}`
      });
    }));

  @Effect()
  installContentItem$ = this.actions$.pipe(
    ofType(CdsActionTypes.INSTALL_CONTENT_ITEM),
    mergeMap( (action: InstallContentItem) =>
      this.requests.installContentItem(action.payload.id, action.payload.user).pipe(
        map( _ => new InstallContentItemSuccess( )),
        catchError((error) => of(new InstallContentItemFailure(error))))
    ));

  @Effect()
  installContentItemFailure$ = this.actions$.pipe(
    ofType(CdsActionTypes.INSTALL_CONTENT_ITEM_FAILURE),
    map(({ payload: { error } }: InstallContentItemFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Error installing content item errors: ${msg || error}`
      });
    }));

  @Effect()
  installContentItemSuccess$ = this.actions$.pipe(
    ofType(CdsActionTypes.INSTALL_CONTENT_ITEM_SUCCESS),
    map((_action: InstallContentItemSuccess) => {
      return new CreateNotification({
        type: Type.info,
        message: 'Content Item was installed'
      });
    }));

  @Effect()
  downloadContentItem$ = this.actions$.pipe(
    ofType(CdsActionTypes.DOWNLOAD_CONTENT_ITEM),
    mergeMap( (action: DownloadContentItem) =>
      this.requests.downloadContentItem(action.payload.id).pipe(
        map( (file: Blob) => {
          saveAs(file, action.payload.filename);
          return new DownloadContentItemSuccess( { name: action.payload.name } );
        }),
        catchError((error) => of(new DownloadContentItemFailure(
          { name: action.payload.name, httpErrorResponse: error})))
    )));

  @Effect()
  downloadContentItemFailure$ = this.actions$.pipe(
    ofType(CdsActionTypes.DOWNLOAD_CONTENT_ITEM_FAILURE),
    map(( action: DownloadContentItemFailure) => {
      const error = action.payload.httpErrorResponse.error;
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Error downloading content item "${action.payload.name}" errors: ${msg || error}`
      });
    }));

  @Effect()
  downloadContentItemSuccess$ = this.actions$.pipe(
    ofType(CdsActionTypes.DOWNLOAD_CONTENT_ITEM_SUCCESS),
    map((action: DownloadContentItemSuccess) => {
      return new CreateNotification({
        type: Type.info,
        message: `Content Item "${action.payload.name}" was download`
      });
    }));

  @Effect()
  isContentEnabled$ = this.actions$.pipe(
    ofType(CdsActionTypes.IS_CONTENT_ENABLED),
    mergeMap( (_action) =>
      this.requests.isContentEnabled().pipe(
        map(isContentEnabled => new IsContentEnabledSuccess( isContentEnabled )),
        catchError((error) => of(new IsContentEnabledFailure(error))))
    ));

  @Effect()
  isContentEnabledFailure$ = this.actions$.pipe(
    ofType(CdsActionTypes.IS_CONTENT_ENABLED_FAILURE),
    map(({ payload: { error } }: IsContentEnabledFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not check is content is enabled: ${msg || error}`
      });
    }));
}
