
import { createSelector, createFeatureSelector } from '@ngrx/store';

import { NotificationRuleEntityState, notificationRuleEntityAdapter } from './notification_rule.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const notificationRuleState = createFeatureSelector<NotificationRuleEntityState>('notificationRules');

export const {
  selectAll: allRules,
  selectEntities: ruleEntities
} = notificationRuleEntityAdapter.getSelectors(notificationRuleState);

export const notificationRuleStatus = createSelector(
  notificationRuleState,
  (state) => state.rulesStatus
);

export const getAllStatus = createSelector(
  notificationRuleState,
  (state) => state.getAllStatus
);

export const getStatus = createSelector(
  notificationRuleState,
  (state) => state.getStatus
);

export const notificationFromRoute = createSelector(
  ruleEntities,
  routeParams,
  (state, { id }) => find({ id }, state)
);

export const updateStatus = createSelector(
  notificationRuleState,
  (state) => state.updateStatus
);
