
import { createSelector, createFeatureSelector } from '@ngrx/store';

import { NotificationRuleEntityState, notificationRuleEntityAdapter } from './notification_rule.reducer';

export const ruleState = createFeatureSelector<NotificationRuleEntityState>('rules');

export const {
  selectAll: allRules,
  selectEntities: ruleEntities
} = notificationRuleEntityAdapter.getSelectors(ruleState);

export const deleteStatus = createSelector(
  ruleState,
  (state) => state.deleteStatus
);
