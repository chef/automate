import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { HttpErrorResponse } from '@angular/common/http';
import { set, pipe, unset } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { RuleActionTypes, RuleActions } from './rule.actions';
import { Rule } from './rule.model';

export interface RuleEntityState extends EntityState<Rule> {
  getAttributes: any;
  getAllStatus: EntityStatus;
  getStatus: EntityStatus;
  createStatus: EntityStatus;
  createError: HttpErrorResponse;
  updateStatus: EntityStatus;
  deleteStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';
const GET_STATUS = 'getStatus';
const CREATE_STATUS = 'createStatus';
const CREATE_ERROR = 'createError';
const DELETE_STATUS = 'deleteStatus';
const UPDATE_STATUS = 'updateStatus';

export const Attributes = {
  node: [
    {
      key: 'CHEF_ORGS',
      value: 'Chef Organization'
    },
    {
      key: 'CHEF_SERVER',
      value: 'Chef Server'
    },
    {
      key: 'CHEF_ENV',
      value: 'Environment'
    },
    {
      key: 'CHEF_ROLE',
      value: 'Chef Role'
    },
    {
      key: 'CHEF_TAG',
      value: 'Chef Tag'
    },
    {
      key: 'CHEF_POLICY_NAME',
      value: 'Chef Policy Name'
    },
    {
      key: 'CHEF_POLICY_GROUP',
      value: 'Chef Policy Group'
    }
  ],
  event: [
    {
      key: 'CHEF_ORGS',
      value: 'Chef Organization'
    },
    {
      key: 'CHEF_SERVER',
      value: 'Chef Server'
    }
  ]
};

export const ruleEntityAdapter: EntityAdapter<Rule> = createEntityAdapter<Rule>();

export const RuleEntityInitialState: RuleEntityState = ruleEntityAdapter.getInitialState({
  getAllStatus: EntityStatus.notLoaded,
  getStatus: EntityStatus.notLoaded,
  createStatus: EntityStatus.notLoaded,
  createError: null,
  deleteStatus: EntityStatus.notLoaded,
  updateStatus: EntityStatus.notLoaded,
  getAttributes: {
    node: [
      {
        key: 'CHEF_ORGS',
        value: 'Chef Organization'
      },
      {
        key: 'CHEF_SERVER',
        value: 'Chef Server'
      },
      {
        key: 'CHEF_ENV',
        value: 'Environment'
      },
      {
        key: 'CHEF_ROLE',
        value: 'Chef Role'
      },
      {
        key: 'CHEF_TAG',
        value: 'Chef Tag'
      },
      {
        key: 'CHEF_POLICY_NAME',
        value: 'Chef Policy Name'
      },
      {
        key: 'CHEF_POLICY_GROUP',
        value: 'Chef Policy Group'
      }
    ],
    event: [
      {
        key: 'CHEF_ORGS',
        value: 'Chef Organization'
      },
      {
        key: 'CHEF_SERVER',
        value: 'Chef Server'
      }
    ]
  }
});

export function ruleEntityReducer(
  state: RuleEntityState = RuleEntityInitialState,
  action: RuleActions): RuleEntityState {

  switch (action.type) {
    case RuleActionTypes.GET_ALL:
      return set(GET_ALL_STATUS, EntityStatus.loading, state);

    case RuleActionTypes.GET_ALL_SUCCESS:
      return set(GET_ALL_STATUS, EntityStatus.loadingSuccess,
        ruleEntityAdapter.addAll(action.payload.rules, state));

    case RuleActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    case RuleActionTypes.GET:
      return set(GET_STATUS, EntityStatus.loading, state);

    case RuleActionTypes.GET_SUCCESS:
      return set(GET_STATUS, EntityStatus.loadingSuccess,
        ruleEntityAdapter.addOne(action.payload.rule, state));

    case RuleActionTypes.GET_FAILURE:
      return set(GET_STATUS, EntityStatus.loadingFailure, state);

    case RuleActionTypes.CREATE:
      return set(CREATE_STATUS, EntityStatus.loading, state);

    case RuleActionTypes.CREATE_SUCCESS:
      return pipe(
        set(CREATE_STATUS, EntityStatus.loadingSuccess),
        unset(CREATE_ERROR)
      )(ruleEntityAdapter.addOne(action.payload.rule, state)) as RuleEntityState;

    case RuleActionTypes.CREATE_FAILURE:
      return pipe(
        set(CREATE_STATUS, EntityStatus.loadingFailure),
        set(CREATE_ERROR, action.payload)
      )(state) as RuleEntityState;

    case RuleActionTypes.DELETE:
      return set(DELETE_STATUS, EntityStatus.loading, state);

    case RuleActionTypes.DELETE_SUCCESS:
      return set(DELETE_STATUS, EntityStatus.loadingSuccess,
        ruleEntityAdapter.removeOne(action.payload.id, state));

    case RuleActionTypes.DELETE_FAILURE:
      return set(DELETE_STATUS, EntityStatus.loadingFailure, state);

    case RuleActionTypes.UPDATE:
      return set(UPDATE_STATUS, EntityStatus.loading, state);

    case RuleActionTypes.UPDATE_SUCCESS:
      return set(UPDATE_STATUS, EntityStatus.loadingSuccess,
        ruleEntityAdapter.updateOne({
          id: action.payload.rule.id,
          changes: action.payload.rule
        }, state));

    case RuleActionTypes.UPDATE_FAILURE:
      return set(UPDATE_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}
