import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';

import { Rule, ProjectStatus } from './rule.model';

export enum RuleActionTypes {
  GET_ALL         = 'RULES::GET_ALL',
  GET_ALL_SUCCESS = 'RULES::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'RULES::GET_ALL::FAILURE',
  GET             = 'RULES::GET',
  GET_SUCCESS     = 'RULES::GET::SUCCESS',
  GET_FAILURE     = 'RULES::GET::FAILURE',
  CREATE          = 'RULES::CREATE',
  CREATE_SUCCESS  = 'RULES::CREATE::SUCCESS',
  CREATE_FAILURE  = 'RULES::CREATE::FAILURE',
  DELETE          = 'RULES::DELETE',
  DELETE_SUCCESS  = 'RULES::DELETE::SUCCESS',
  DELETE_FAILURE  = 'RULES::DELETE::FAILURE',
  UPDATE          = 'RULES::UPDATE',
  UPDATE_SUCCESS  = 'RULES::UPDATE::SUCCESS',
  UPDATE_FAILURE  = 'RULES::UPDATE::FAILURE'
}

export interface RuleSuccessPayload {
  rule: Rule;
}

export class GetRulesForProject implements Action {
  readonly type = RuleActionTypes.GET_ALL;

  constructor(public payload: { project_id: string }) { }
}

export interface RulesSuccessPayload {
  rules: Rule[];
  status: ProjectStatus;
}

export class GetRulesSuccess implements Action {
  readonly type = RuleActionTypes.GET_ALL_SUCCESS;

  constructor(public payload: RulesSuccessPayload) { }
}

export class GetRulesFailure implements Action {
  readonly type = RuleActionTypes.GET_ALL_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class GetRule implements Action {
  readonly type = RuleActionTypes.GET;

  constructor(public payload: { project_id: string, id: string }) { }
}

export class GetRuleSuccess implements Action {
  readonly type = RuleActionTypes.GET_SUCCESS;

  constructor(public payload: RuleSuccessPayload) { }
}

export class GetRuleFailure implements Action {
  readonly type = RuleActionTypes.GET_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class CreateRule implements Action {
  readonly type = RuleActionTypes.CREATE;
  constructor(public payload: { rule: Rule }) { }
}

export class CreateRuleSuccess implements Action {
  readonly type = RuleActionTypes.CREATE_SUCCESS;
  constructor(public payload: RuleSuccessPayload) { }
}

export class CreateRuleFailure implements Action {
  readonly type = RuleActionTypes.CREATE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class DeleteRule implements Action {
  readonly type = RuleActionTypes.DELETE;

  constructor(public payload: { project_id: string, id: string }) { }
}

export class DeleteRuleSuccess implements Action {
  readonly type = RuleActionTypes.DELETE_SUCCESS;

  constructor(public payload: { id: string }) { }
}

export class DeleteRuleFailure implements Action {
  readonly type = RuleActionTypes.DELETE_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateRule implements Action {
  readonly type = RuleActionTypes.UPDATE;

  constructor(public payload: { rule: Rule }) { }
}

export class UpdateRuleSuccess implements Action {
  readonly type = RuleActionTypes.UPDATE_SUCCESS;

  constructor(public payload: RuleSuccessPayload) { }
}

export class UpdateRuleFailure implements Action {
  readonly type = RuleActionTypes.UPDATE_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export type RuleActions =
  | GetRulesForProject
  | GetRulesSuccess
  | GetRulesFailure
  | GetRule
  | GetRuleSuccess
  | GetRuleFailure
  | CreateRule
  | CreateRuleSuccess
  | CreateRuleFailure
  | DeleteRule
  | DeleteRuleSuccess
  | DeleteRuleFailure
  | UpdateRule
  | UpdateRuleSuccess
  | UpdateRuleFailure;
