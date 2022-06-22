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

}
