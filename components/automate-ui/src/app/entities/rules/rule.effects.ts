import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map, filter } from 'rxjs/operators';

import { HttpStatus } from 'app/types/types';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetRulesForProject,
  GetRulesSuccess,
  RulesSuccessPayload,
  GetRulesFailure,
  GetRule,
  GetRuleSuccess,
  GetRuleFailure,
  CreateRule,
  CreateRuleSuccess,
  CreateRuleFailure,
  DeleteRule,
  DeleteRuleSuccess,
  DeleteRuleFailure,
  UpdateRule,
  UpdateRuleFailure,
  UpdateRuleSuccess,
  RuleSuccessPayload,
  RuleActionTypes
} from './rule.actions';

import {
  RuleRequests
} from './rule.requests';

@Injectable()
export class RuleEffects {
  constructor(
    private actions$: Actions,
    private requests: RuleRequests
  ) { }

  getRulesForProject$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RuleActionTypes.GET_ALL),
      mergeMap(({ payload: { project_id } }: GetRulesForProject) =>
        this.requests.getRulesForProject(project_id).pipe(
          map((resp: RulesSuccessPayload) => new GetRulesSuccess(resp)),
          catchError((error: HttpErrorResponse) => observableOf(new GetRulesFailure(error)))))));

  getRulesFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RuleActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetRulesFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get rules: ${msg || payload.error}`
        });
      })));

  getRule$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RuleActionTypes.GET),
      mergeMap(({ payload: { project_id, id } }: GetRule) =>
        this.requests.getRule(project_id, id).pipe(
          map((resp: RuleSuccessPayload) => new GetRuleSuccess(resp)),
          catchError((error: HttpErrorResponse) => observableOf(new GetRuleFailure(error)))))));

  getRuleFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RuleActionTypes.GET_FAILURE),
      map(({ payload }: GetRuleFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get rule: ${msg || payload.error}`
        });
      })));

  createRule$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RuleActionTypes.CREATE),
      mergeMap(({ payload: { rule } }: CreateRule) =>
      this.requests.createRule(rule).pipe(
        map((resp: RuleSuccessPayload) => new CreateRuleSuccess(resp)),
        catchError((error: HttpErrorResponse) => observableOf(new CreateRuleFailure(error)))))));

  createRuleSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RuleActionTypes.CREATE_SUCCESS),
      map(({ payload: { rule } }: CreateRuleSuccess) => new CreateNotification({
      type: Type.info,
      message: `Created rule ${rule.name}`
    }))));

  createRuleFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(RuleActionTypes.CREATE_FAILURE),
    filter(({ payload }: CreateRuleFailure) => payload.status !== HttpStatus.CONFLICT),
    map(({ payload }: CreateRuleFailure) => new CreateNotification({
        type: Type.error,
        message: `Could not create rule: ${payload.error.error || payload}`
      }))));

  deleteRule$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RuleActionTypes.DELETE),
      mergeMap(({ payload: { project_id, id } }: DeleteRule) =>
        this.requests.deleteRule(project_id, id).pipe(
          map(() => new DeleteRuleSuccess({id})),
          catchError((error: HttpErrorResponse) =>
            observableOf(new DeleteRuleFailure(error)))))));

  deleteRuleSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RuleActionTypes.DELETE_SUCCESS),
      map(({ payload: { id } }: DeleteRuleSuccess) => {
        return new CreateNotification({
          type: Type.info,
          message: `Deleted rule ${id}.`
        });
      })));

  deleteRuleFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RuleActionTypes.DELETE_FAILURE),
      map(({ payload }: DeleteRuleFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not delete rule: ${msg || payload.error}`
        });
      })));

  updateRule$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RuleActionTypes.UPDATE),
      mergeMap(({ payload: { rule } }: UpdateRule) =>
        this.requests.updateRule(rule).pipe(
          map((resp: RuleSuccessPayload) => new UpdateRuleSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new UpdateRuleFailure(error)))))));

  updateRuleFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RuleActionTypes.UPDATE_FAILURE),
      map(({ payload }: UpdateRuleFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not update rule: ${msg || payload.error}`
        });
      })));
}

