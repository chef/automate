import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map, filter } from 'rxjs/operators';

import { HttpStatus } from 'app/types/types';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetOrgs,
  GetOrgsSuccess,
  OrgsSuccessPayload,
  GetOrgsFailure,
  GetOrg,
  GetOrgSuccess,
  GetOrgFailure,
  CreateOrg,
  CreateOrgSuccess,
  CreateOrgFailure,
  DeleteOrg,
  DeleteOrgSuccess,
  DeleteOrgFailure,
  UpdateOrg,
  UpdateOrgFailure,
  UpdateOrgSuccess,
  OrgSuccessPayload,
  UploadZip,
  UploadZipSuccess,
  UploadZipFailure,
  UploadSuccessPayload,
  CancelMigration,
  CancelMigrationSuccess,
  CancelMigrationFailure,
  GetPreviewData,
  GetPreviewDataSuccess,
  GetPreviewDataFailure,
  ConfirmPreview,
  ConfirmPreviewSuccess,
  ConfirmPreviewFailure,
  CheckUser,
  CheckUserSuccess,
  CheckUserFailure,
  OrgActionTypes
} from './org.actions';

import {
  OrgRequests
} from './org.requests';

@Injectable()
export class OrgEffects {
  constructor(
    private actions$: Actions,
    private requests: OrgRequests
  ) { }

  getOrgsForProject$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.GET_ALL),
      mergeMap(({ payload: { server_id } }: GetOrgs) =>
        this.requests.getOrgs(server_id).pipe(
          map((resp: OrgsSuccessPayload) => new GetOrgsSuccess(resp)),
          catchError((error: HttpErrorResponse) => observableOf(new GetOrgsFailure(error)))))));

  getOrgsFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetOrgsFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get organizations: ${msg || payload.error}`
        });
      })));

  getOrg$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.GET),
      mergeMap(({ payload: { server_id, id } }: GetOrg) =>
        this.requests.getOrg(server_id, id).pipe(
          map((resp: OrgSuccessPayload) => new GetOrgSuccess(resp)),
          catchError((error: HttpErrorResponse) => observableOf(new GetOrgFailure(error)))))));

  getOrgFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.GET_FAILURE),
      map(({ payload }: GetOrgFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get organization: ${msg || payload.error}`
        });
      })));

  createOrg$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.CREATE),
      mergeMap(({ payload }: CreateOrg) =>
      this.requests.createOrg( payload ).pipe(
        map((resp: OrgSuccessPayload) => new CreateOrgSuccess(resp)),
        catchError((error: HttpErrorResponse) => observableOf(new CreateOrgFailure(error)))))));

  createOrgSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.CREATE_SUCCESS),
      map(({ payload: { org } }: CreateOrgSuccess) => new CreateNotification({
      type: Type.info,
      message: `Successfully created organization - ${org.name}.`
    }))));

  createOrgFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(OrgActionTypes.CREATE_FAILURE),
    filter(({ payload }: CreateOrgFailure) => payload.status !== HttpStatus.CONFLICT),
    map(({ payload }: CreateOrgFailure) => new CreateNotification({
        type: Type.error,
        message: `Could not create organization: ${payload.error.error || payload}`
      }))));

  deleteOrg$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.DELETE),
      mergeMap(({ payload: { server_id, id, name } }: DeleteOrg) =>
        this.requests.deleteOrg(server_id, id).pipe(
          map(() => new DeleteOrgSuccess({id, name})),
          catchError((error: HttpErrorResponse) =>
            observableOf(new DeleteOrgFailure(error)))))));

  deleteOrgSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.DELETE_SUCCESS),
      map(({ payload: { name } }: DeleteOrgSuccess) => {
        return new CreateNotification({
          type: Type.info,
          message: `Successfully deleted organization ${name}.`
        });
      })));

  deleteOrgFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.DELETE_FAILURE),
      map(({ payload }: DeleteOrgFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not delete organization: ${msg || payload.error}`
        });
      })));

  updateOrg$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.UPDATE),
      mergeMap(({ payload: { org } }: UpdateOrg) =>
        this.requests.updateOrg(org).pipe(
          map((resp: OrgSuccessPayload) => new UpdateOrgSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new UpdateOrgFailure(error)))))));

  updateOrgSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.UPDATE_SUCCESS),
      map(({ payload: { org } }: UpdateOrgSuccess) => new CreateNotification({
      type: Type.info,
      message: `Successfully updated organization ${org.name}.`
    }))));

  updateOrgFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.UPDATE_FAILURE),
      map(({ payload }: UpdateOrgFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not update organization: ${msg || payload.error}`
        });
      })));

  uploadZip$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.UPLOAD),
      mergeMap(({ payload: { formData } }: UploadZip) =>
        this.requests.uploadZip(formData).pipe(
          map((resp: UploadSuccessPayload) => new UploadZipSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new UploadZipFailure(error)))))));

  uploadZipSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.UPLOAD_SUCCESS),
      map((_UploadZipSuccess) => new CreateNotification({
      type: Type.info,
      message: 'Successfully uploaded file.'
    }))));

  uploadZipFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.UPLOAD_FAILURE),
      map(({ payload }: UploadZipFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not upload file: ${msg || payload.error}`
        });
    })));

  cancelMigration$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.CANCEL_MIGRATION),
      mergeMap(({ payload:  { server_id, migration_id } }: CancelMigration) =>
        this.requests.cancelMigration(server_id, migration_id).pipe(
          map((resp) => new CancelMigrationSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new CancelMigrationFailure(error)))))));

  cancelMigrationSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.CANCEL_MIGRATION_SUCCESS),
      map((_) => new CreateNotification({
      type: Type.info,
      message: 'Cancelled migration.'
    }))));

  cancelMigrationFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.CANCEL_MIGRATION_FAILURE),
      map(({ payload }: CancelMigrationFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not cancel migration: ${msg || payload.error}`
        });
    })));

  getPreviewData$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.GET_PREVIEW_DATA),
      mergeMap(({ payload:  { migration_id } }: GetPreviewData) =>
        this.requests.getPreviewData(migration_id).pipe(
          map((resp) => new GetPreviewDataSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new GetPreviewDataFailure(error)))))));

  getPreviewDataFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.GET_PREVIEW_DATA_FAILURE),
      map(({ payload }: GetPreviewDataFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get preview data: ${msg || payload.error}`
      });
    })));

  confirmPreview$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.CONFIRM_PREVIEW),
      mergeMap(({ payload:  { server_id, migration_id } }: ConfirmPreview) =>
        this.requests.confirmPreview(server_id, migration_id).pipe(
          map((resp) => new ConfirmPreviewSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new ConfirmPreviewFailure(error)))))));

  confirmPreviewSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.CONFIRM_PREVIEW_SUCCESS),
      map((_) => new CreateNotification({
      type: Type.info,
      message: 'Confirm preview successful.'
    }))));

  confirmPreviewFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.CONFIRM_PREVIEW_FAILURE),
      map(({ payload }: ConfirmPreviewFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not confirm preview: ${msg || payload.error}`
        });
    })));

  CheckUser = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.CHECK_USER),
      mergeMap(({ payload:  { user } }: CheckUser) =>
        this.requests.checkUser(user).pipe(
          map((resp) => new CheckUserSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new CheckUserFailure(error)))))));

  CheckUserSuccess = createEffect(() =>
    this.actions$.pipe(
      ofType(OrgActionTypes.CHECK_USER_SUCCESS),
      map(({}: ConfirmPreviewFailure) => {
      return new CreateNotification({
        type: Type.error,
        message: `Could not use this user name`
      });
    })));

  // CheckUserFailure = createEffect(() =>
  // this.actions$.pipe(
  //   ofType(OrgActionTypes.CHECK_USER_FAILURE),
  //   map((_) => new CreateNotification({
  //     type: Type.info,
  //     message: 'Confirm preview successful.'})));
}
