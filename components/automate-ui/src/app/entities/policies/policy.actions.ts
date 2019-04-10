import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';

import { Policy, Member } from './policy.model';
import { IamVersionResponse } from './policy.requests';

export enum PolicyActionTypes {
  GET_IAM_VERSION         = 'POLICY::GET_IAM_VERSION',
  GET_IAM_VERSION_SUCCESS = 'POLICY::GET_IAM_VERSION_SUCCESS',
  GET_IAM_VERSION_FAILURE = 'POLICY::GET_IAM_VERSION_FAILURE',
  GET_ALL                 = 'POLICY::GET_ALL',
  GET_ALL_SUCCESS         = 'POLICY::GET_ALL::SUCCESS',
  GET_ALL_FAILURE         = 'POLICY::GET_ALL::FAILURE',
  GET                     = 'POLICY::GET',
  GET_SUCCESS             = 'POLICY::GET::SUCCESS',
  GET_FAILURE             = 'POLICY::GET::FAILURE',
  REMOVE_MEMBERS          = 'POLICY::MEMBERS::REMOVE',
  REMOVE_MEMBERS_SUCCESS  = 'POLICY::MEMBERS::REMOVE::SUCCESS',
  REMOVE_MEMBERS_FAILURE  = 'POLICY::MEMBERS::REMOVE::FAILURE',
  ADD_MEMBERS             = 'POLICY::MEMBERS::ADD',
  ADD_MEMBERS_SUCCESS     = 'POLICY::MEMBERS::ADD::SUCCESS',
  ADD_MEMBERS_FAILURE     = 'POLICY::MEMBERS::ADD::FAILURE',
  DELETE                  = 'POLICY::DELETE',
  DELETE_SUCCESS          = 'POLICY::DELETE::SUCCESS',
  DELETE_FAILURE          = 'POLICY::DELETE::FAILURE'
}

export class GetIamVersion implements Action {
  readonly type = PolicyActionTypes.GET_IAM_VERSION;
}

export class GetIamVersionSuccess implements Action {
  readonly type = PolicyActionTypes.GET_IAM_VERSION_SUCCESS;
  constructor(public payload: IamVersionResponse) { }
}

export class GetIamVersionFailure implements Action {
  readonly type = PolicyActionTypes.GET_IAM_VERSION_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetPolicies implements Action {
  readonly type = PolicyActionTypes.GET_ALL;
}

export interface GetPoliciesSuccessPayload {
  policies: Policy[];
}

export class GetPoliciesSuccess implements Action {
  readonly type = PolicyActionTypes.GET_ALL_SUCCESS;
  constructor(public payload: GetPoliciesSuccessPayload) { }
}

export class GetPoliciesFailure implements Action {
  readonly type = PolicyActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetPolicy implements Action {
  readonly type = PolicyActionTypes.GET;
  constructor(public payload: { id: string }) { }
}

export class GetPolicySuccess implements Action {
  readonly type = PolicyActionTypes.GET_SUCCESS;
  constructor(public payload: Policy) { }
}

export class GetPolicyFailure implements Action {
  readonly type = PolicyActionTypes.GET_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export interface PolicyMembersMgmtPayload {
  id: string;
  members: Member[];
}

export class RemovePolicyMembers implements Action {
  readonly type = PolicyActionTypes.REMOVE_MEMBERS;
  constructor(public payload: PolicyMembersMgmtPayload) { }
}

export class RemovePolicyMembersSuccess implements Action {
  readonly type = PolicyActionTypes.REMOVE_MEMBERS_SUCCESS;
  constructor(public payload: {
    id: string;
    // Keep things as Members so we can display nice notifications.
    members_removed: Member[];
    // Since we are just shoving this back into the policy,
    // we don't need to do the parsing here. Strings get
    // returned from the API so the app can parse as needed after
    // policy is updated.
    members_left: string[];
  }) { }
}

export class RemovePolicyMembersFailure implements Action {
  readonly type = PolicyActionTypes.REMOVE_MEMBERS_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class AddPolicyMembers implements Action {
  readonly type = PolicyActionTypes.ADD_MEMBERS;
  constructor(public payload: PolicyMembersMgmtPayload) { }
}

export class AddPolicyMembersSuccess implements Action {
  readonly type = PolicyActionTypes.ADD_MEMBERS_SUCCESS;
  constructor(public payload: {
    id: string;
    // Keep things as Members so we can display nice notifications.
    members_added: Member[];
    // Since we are just shoving this back into the policy,
    // we don't need to do the parsing here. Strings get
    // returned from the API so the app can parse as needed after
    // policy is updated.
    resulting_members: string[];
  }) { }
}

export class AddPolicyMembersFailure implements Action {
  readonly type = PolicyActionTypes.ADD_MEMBERS_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class DeletePolicy implements Action {
  readonly type = PolicyActionTypes.DELETE;
  constructor(public payload: { id: string }) { }
}

export class DeletePolicySuccess implements Action {
  readonly type = PolicyActionTypes.DELETE_SUCCESS;
  constructor(public payload: { id: string }) { }
}

export class DeletePolicyFailure implements Action {
  readonly type = PolicyActionTypes.DELETE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}


export type PolicyActions =
  | GetIamVersion
  | GetIamVersionSuccess
  | GetIamVersionFailure
  | GetPolicies
  | GetPoliciesSuccess
  | GetPoliciesFailure
  | GetPolicy
  | GetPolicySuccess
  | GetPolicyFailure
  | RemovePolicyMembers
  | RemovePolicyMembersSuccess
  | RemovePolicyMembersFailure
  | AddPolicyMembers
  | AddPolicyMembersSuccess
  | AddPolicyMembersFailure
  | DeletePolicy
  | DeletePolicySuccess
  | DeletePolicyFailure;
