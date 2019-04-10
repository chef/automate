import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { pipe, unset, set } from 'lodash/fp';
import { HttpErrorResponse } from '@angular/common/http';

import { EntityStatus } from 'app/entities/entities';
import { IAMMajorVersion, IAMMinorVersion } from 'app/entities/policies/policy.model';
import { PolicyActionTypes, PolicyActions } from './policy.actions';
import { Policy } from './policy.model';

export interface PolicyEntityState extends EntityState<Policy> {
  getAllStatus: EntityStatus;
  getStatus: EntityStatus;
  deleteStatus: EntityStatus;
  addPolicyMembersStatus: EntityStatus;
  addPolicyMembersHTTPError: HttpErrorResponse;
  removePolicyMembersStatus: EntityStatus;
  removePolicyMembersHTTPError: HttpErrorResponse;
  iamMajorVersion: IAMMajorVersion;
  iamMinorVersion: IAMMinorVersion;
}

export const policyEntityAdapter: EntityAdapter<Policy> = createEntityAdapter<Policy>();

export const PolicyEntityInitialState: PolicyEntityState = policyEntityAdapter.getInitialState({
  getAllStatus: EntityStatus.notLoaded,
  getStatus: EntityStatus.notLoaded,
  deleteStatus: EntityStatus.notLoaded,
  addPolicyMembersStatus: EntityStatus.notLoaded,
  addPolicyMembersHTTPError: null,
  removePolicyMembersStatus: EntityStatus.notLoaded,
  removePolicyMembersHTTPError: null,
  iamMajorVersion: null,
  iamMinorVersion: null
});

export function policyEntityReducer(state: PolicyEntityState = PolicyEntityInitialState,
  action: PolicyActions): PolicyEntityState {

  switch (action.type) {

    case PolicyActionTypes.GET_IAM_VERSION_SUCCESS:
    return pipe(
      set('iamMajorVersion', action.payload.version.major.toLowerCase()),
      set('iamMinorVersion', action.payload.version.minor.toLowerCase())
      )(state) as PolicyEntityState;

    case PolicyActionTypes.GET_ALL:
      return set('getAllStatus', EntityStatus.loading, state) as PolicyEntityState;

    case PolicyActionTypes.GET_ALL_SUCCESS:
    return set('getAllStatus', EntityStatus.loadingSuccess,
        policyEntityAdapter.addAll(action.payload.policies, state)) as PolicyEntityState;

    case PolicyActionTypes.GET_ALL_FAILURE:
      return set('getAllStatus', EntityStatus.loadingFailure, state) as PolicyEntityState;

    case PolicyActionTypes.GET:
      return set('getStatus', EntityStatus.loading, state) as PolicyEntityState;

    case PolicyActionTypes.GET_SUCCESS:
      return set('getStatus', EntityStatus.loadingSuccess,
        policyEntityAdapter.addOne(action.payload, state)) as PolicyEntityState;

    case PolicyActionTypes.GET_FAILURE:
      return set('getStatus', EntityStatus.loadingFailure, state) as PolicyEntityState;

    case PolicyActionTypes.REMOVE_MEMBERS:
      return set('removePolicyMembersStatus', EntityStatus.loading, state) as PolicyEntityState;

    case PolicyActionTypes.REMOVE_MEMBERS_SUCCESS: {
      return pipe(
        unset('removePolicyMembersHTTPError'),
        set('removePolicyMembersStatus', EntityStatus.loadingSuccess)
      )(policyEntityAdapter.updateOne({
        id: action.payload.id,
        changes: { ...state.entities[action.payload.id], members: action.payload.members_left }
      }, state)) as PolicyEntityState;
    }

    case PolicyActionTypes.REMOVE_MEMBERS_FAILURE:
      return pipe(
        set('removePolicyMembersHTTPError', action.payload),
        set('removePolicyMembersStatus', EntityStatus.loadingFailure)
      )(state) as PolicyEntityState;

    case PolicyActionTypes.ADD_MEMBERS:
      return set('addPolicyMembersStatus', EntityStatus.loading, state) as PolicyEntityState;

    case PolicyActionTypes.ADD_MEMBERS_SUCCESS: {
      const clone = Object.assign({}, state.entities[action.payload.id]);
      clone.members = action.payload.resulting_members;
      return pipe(
        unset('addPolicyMembersHTTPError'),
        set('addPolicyMembersStatus', EntityStatus.loadingSuccess)
      )(policyEntityAdapter.updateOne({
        id: action.payload.id,
        changes: clone
      }, state)) as PolicyEntityState;
    }

    case PolicyActionTypes.ADD_MEMBERS_FAILURE:
      return pipe(
        set('addPolicyMembersHTTPError', action.payload),
        set('addPolicyMembersStatus', EntityStatus.loadingFailure)
      )(state) as PolicyEntityState;

    case PolicyActionTypes.DELETE:
      return set('deleteStatus', EntityStatus.loading, state) as PolicyEntityState;

    case PolicyActionTypes.DELETE_SUCCESS:
      return set('deleteStatus', EntityStatus.loadingSuccess,
        policyEntityAdapter.removeOne(action.payload.id, state)) as PolicyEntityState;

    case PolicyActionTypes.DELETE_FAILURE:
      return set('deleteStatus', EntityStatus.loadingFailure, state) as PolicyEntityState;

    default:
      return state;

  }
}
