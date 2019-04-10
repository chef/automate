import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';

import { memberListToStringList, policyFromPayload } from './policy.model';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetIamVersion,
  GetIamVersionSuccess,
  GetIamVersionFailure,
  GetPolicies,
  GetPoliciesSuccess,
  GetPoliciesFailure,
  GetPolicy,
  GetPolicySuccess,
  GetPolicyFailure,
  PolicyActionTypes,
  RemovePolicyMembers,
  RemovePolicyMembersSuccess,
  RemovePolicyMembersFailure,
  AddPolicyMembers,
  AddPolicyMembersSuccess,
  AddPolicyMembersFailure,
  DeletePolicy,
  DeletePolicySuccess,
  DeletePolicyFailure,
  PolicyActions
} from './policy.actions';

import {
  PolicyRequests,
  MembersResponse
} from './policy.requests';

@Injectable()
export class PolicyEffects {
  constructor(
    private actions$: Actions<PolicyActions>,
    private requests: PolicyRequests
  ) { }

  // Note 2019/02/12 (sr): When upgrading to ngrx 7.x.y, we should be able to
  // remove all the forced types on `ofType` -- with this commit[1], they should
  // be inferred automatically from the type of `actions$`, that is
  // `Actions<PolicyActions>`.
  //
  // [1]: https://github.com/ngrx/platform/commit/8d56a6f7e9a0158d30c6a0f335f9805cc7d5a555

  @Effect()
  getiamMajorVersion$ = this.actions$.pipe(
    ofType<GetIamVersion>(PolicyActionTypes.GET_IAM_VERSION),
    mergeMap(() => this.requests.getIamVersion().pipe(
      map(resp => new GetIamVersionSuccess(resp)),
      catchError((error: HttpErrorResponse) => observableOf(new GetIamVersionFailure(error))))));

  @Effect()
  getIamVersionFailure$ = this.actions$.pipe(
    ofType<GetIamVersionFailure>(PolicyActionTypes.GET_IAM_VERSION_FAILURE),
    map(({ payload: { error } }) => new CreateNotification({
      type: Type.error,
      message: `Could not get IAM version: ${error.error || error}.`
    })));

  @Effect()
  getPolicies$ = this.actions$.pipe(
    ofType<GetPolicies>(PolicyActionTypes.GET_ALL),
    mergeMap(() => this.requests.getPolicies().pipe(
      map(({ policies }) => new GetPoliciesSuccess({ policies: policies.map(policyFromPayload) })),
      catchError((error: HttpErrorResponse) => observableOf(new GetPoliciesFailure(error))))));

  @Effect()
  getPoliciesFailure$ = this.actions$.pipe(
    ofType<GetPoliciesFailure>(PolicyActionTypes.GET_ALL_FAILURE),
    map(({ payload: { error } }) => new CreateNotification({
      type: Type.error,
      message: `Could not get policies: ${error.error || error}`
    })));

  @Effect()
  getPolicy$ = this.actions$.pipe(
    ofType<GetPolicy>(PolicyActionTypes.GET),
    mergeMap(({ payload: { id }}) =>
      this.requests.getPolicy(id).pipe(
        map(({ policy }) => new GetPolicySuccess(policyFromPayload(policy))),
        catchError((error: HttpErrorResponse) => observableOf(new GetPoliciesFailure(error))))));

  @Effect()
  getPolicyFailure$ = this.actions$.pipe(
    ofType<GetPolicyFailure>(PolicyActionTypes.GET_FAILURE),
    map(({ payload: { error } }) => new CreateNotification({
      type: Type.error,
      message: `Could not get policy: ${error.error || error}`
    })));

  @Effect()
  removePolicyMembers$ = this.actions$.pipe(
    ofType<RemovePolicyMembers>(PolicyActionTypes.REMOVE_MEMBERS),
    mergeMap(({ payload: { id, members }}) =>
      this.requests.removePolicyMembers(id, memberListToStringList(members)).pipe(
        map(resp => new RemovePolicyMembersSuccess({
          id,
          members_removed: members,
          members_left: resp.members})),
        catchError((error: HttpErrorResponse) =>
          observableOf(new RemovePolicyMembersFailure(error))))));

  @Effect()
  removePolicyMembersSuccess$ = this.actions$.pipe(
    ofType<RemovePolicyMembersSuccess>(PolicyActionTypes.REMOVE_MEMBERS_SUCCESS),
    map(({ payload: { members_removed: removed } }) => {
      const removeStr = removed.length === 1 ? removed[0].displayName : `${removed.length} members`;
      return new CreateNotification({
          type: Type.info,
          message: `Removed ${removeStr}.`
      });
    }));

  @Effect()
  removePolicyMembersFailure$ = this.actions$.pipe(
    ofType<RemovePolicyMembersFailure>(PolicyActionTypes.REMOVE_MEMBERS_FAILURE),
    map(({ payload: { error } }) => new CreateNotification({
      type: Type.error,
      message: `Could not remove members from policy: ${error.error || error}`
    })));

  @Effect()
  addPolicyMembers$ = this.actions$.pipe(
    ofType<AddPolicyMembers>(PolicyActionTypes.ADD_MEMBERS),
    mergeMap(({ payload: { id, members }}) =>
      this.requests.addPolicyMembers(id, memberListToStringList(members)).pipe(
        map((resp: MembersResponse) => new AddPolicyMembersSuccess({
          id,
          members_added: members,
          resulting_members: resp.members
        })),
        catchError((error: HttpErrorResponse) =>
          observableOf(new AddPolicyMembersFailure(error))))));

  @Effect()
  addPolicyMembersSuccess$ = this.actions$.pipe(
    ofType<AddPolicyMembersSuccess>(PolicyActionTypes.ADD_MEMBERS_SUCCESS),
    map(({ payload: { members_added: added } }) => {
      const addedStr = added.length === 1 ? added[0].displayName : `${added.length} members`;
      return new CreateNotification({
          type: Type.info,
          message: `Added ${addedStr}.`
      });
    }));

  @Effect()
  deletePolicy$ = this.actions$.pipe(
    ofType<DeletePolicy>(PolicyActionTypes.DELETE),
    mergeMap(({ payload: { id } }) =>
      this.requests.deletePolicy(id).pipe(
        map(() => new DeletePolicySuccess({ id })),
        catchError((error: HttpErrorResponse) => observableOf(new DeletePolicyFailure(error))))));

  @Effect()
  deletePolicySuccess$ = this.actions$.pipe(
    ofType<DeletePolicySuccess>(PolicyActionTypes.DELETE_SUCCESS),
    map(({ payload: { id } }) => new CreateNotification({
      type: Type.info,
      message: `Deleted policy ${id}.`
    })));

  @Effect()
  deletePolicyFailure$ = this.actions$.pipe(
    ofType<DeletePolicyFailure>(PolicyActionTypes.DELETE_FAILURE),
    map(({ payload: { error } }) =>  new CreateNotification({
      type: Type.error,
      message: `Could not delete policy: ${error.error || error}`
    })));
}

