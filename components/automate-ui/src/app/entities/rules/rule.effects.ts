import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map, filter } from 'rxjs/operators';

import { HttpStatus } from 'app/types/types';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetRulesForProject,
  GetRulesSuccess,
  GetRulesSuccessPayload,
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

  @Effect()
  getRulesForProject$ = this.actions$.pipe(
      ofType(RuleActionTypes.GET_ALL),
      mergeMap(({ payload: { project_id } }: GetRulesForProject) =>
        this.requests.getRulesForProject(project_id).pipe(
          map((resp: GetRulesSuccessPayload) => new GetRulesSuccess(resp)),
          catchError((error: HttpErrorResponse) => observableOf(new GetRulesFailure(error))))));

  @Effect()
  getRulesFailure$ = this.actions$.pipe(
      ofType(RuleActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetRulesFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get rules: ${msg || payload.error}`
        });
      }));

  @Effect()
  getRule$ = this.actions$.pipe(
      ofType(RuleActionTypes.GET),
      mergeMap(({ payload: { id } }: GetRule) =>
        this.requests.getRule(id).pipe(
          map((resp: RuleSuccessPayload) => new GetRuleSuccess(resp)),
          catchError((error: HttpErrorResponse) => observableOf(new GetRuleFailure(error))))));

 @Effect()
  getRuleFailure$ = this.actions$.pipe(
      ofType(RuleActionTypes.GET_FAILURE),
      map(({ payload }: GetRuleFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get rule: ${msg || payload.error}`
        });
      }));

  @Effect()
  createRule$ = this.actions$.pipe(
      ofType(RuleActionTypes.CREATE),
      mergeMap(({ payload: { rule } }: CreateRule) =>
      this.requests.createRule(rule).pipe(
        map((resp: RuleSuccessPayload) => new CreateRuleSuccess(resp)),
        catchError((error: HttpErrorResponse) => observableOf(new CreateRuleFailure(error))))));

  @Effect()
  createRuleSuccess$ = this.actions$.pipe(
      ofType(RuleActionTypes.CREATE_SUCCESS),
      map(({ payload: { rule } }: CreateRuleSuccess) => new CreateNotification({
      type: Type.info,
      message: `Created rule ${rule.id}`
    })));

  @Effect()
  createRuleFailure$ = this.actions$.pipe(
    ofType(RuleActionTypes.CREATE_FAILURE),
    filter(({ payload }: CreateRuleFailure) => payload.status !== HttpStatus.CONFLICT),
    map(({ payload }: CreateRuleFailure) => new CreateNotification({
        type: Type.error,
        message: `Could not create rule: ${payload.error.error || payload}`
      })));

  @Effect()
  deleteRule$ = this.actions$.pipe(
      ofType(RuleActionTypes.DELETE),
      mergeMap(({ payload: { id } }: DeleteRule) =>
        this.requests.deleteRule(id).pipe(
          map(() => new DeleteRuleSuccess({id})),
          catchError((error: HttpErrorResponse) =>
            observableOf(new DeleteRuleFailure(error))))));

  @Effect()
  deleteRuleSuccess$ = this.actions$.pipe(
      ofType(RuleActionTypes.DELETE_SUCCESS),
      map(({ payload: { id } }: DeleteRuleSuccess) => {
        return new CreateNotification({
          type: Type.info,
          message: `Deleted rule ${id}.`
        });
      }));

  @Effect()
  deleteRuleFailure$ = this.actions$.pipe(
      ofType(RuleActionTypes.DELETE_FAILURE),
      map(({ payload }: DeleteRuleFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not delete rule: ${msg || payload.error}`
        });
      }));

  @Effect()
  updateRule$ = this.actions$.pipe(
      ofType(RuleActionTypes.UPDATE),
      mergeMap(({ payload: { rule } }: UpdateRule) =>
        this.requests.updateRule(rule).pipe(
          map((resp: RuleSuccessPayload) => new UpdateRuleSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new UpdateRuleFailure(error))))));

  @Effect()
  updateRuleFailure$ = this.actions$.pipe(
      ofType(RuleActionTypes.UPDATE_FAILURE),
      map(({ payload }: UpdateRuleFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not update rule: ${msg || payload.error}`
        });
      }));
}

