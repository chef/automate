
import { createSelector, createFeatureSelector } from '@ngrx/store';

import { routeParams } from 'app/route.selectors';
import { RuleEntityState, ruleEntityAdapter } from './rule.reducer';

export const ruleState = createFeatureSelector<RuleEntityState>('rules');

export const {
  selectAll: allRules,
  selectEntities: ruleEntities
} = ruleEntityAdapter.getSelectors(ruleState);

export const getAllStatus = createSelector(
  ruleState,
  (state) => state.getAllStatus
);

export const getStatus = createSelector(
  ruleState,
  (state) => state.getStatus
);

export const createStatus = createSelector(
  ruleState,
  (state) => state.createStatus
);

export const createError = createSelector(
  ruleState,
  (state) => state.createError
);


export const updateStatus = createSelector(
  ruleState,
  (state) => state.updateStatus
);

export const ruleFromRoute = createSelector(
  ruleEntities,
  routeParams,
  (state, { ruleid }) => state[ruleid]
);
