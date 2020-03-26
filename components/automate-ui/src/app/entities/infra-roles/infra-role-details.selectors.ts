import { createSelector, createFeatureSelector } from '@ngrx/store';
import { InfraRoleDetailsEntityState, infraRoleDetailsEntityAdapter } from './infra-role-details.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const infraRoleState =
createFeatureSelector<InfraRoleDetailsEntityState>('infraroledetails');

export const {
  selectAll: allInfraRoles,
  selectEntities: roleEntities
} = infraRoleDetailsEntityAdapter.getSelectors(infraRoleState);

export const infraRoleStatus = createSelector(
  infraRoleState,
  (state) => state.status
);

export const getStatus = createSelector(
  infraRoleState,
  (state) => state.getStatus
);

export const infaRoleFromRoute = createSelector(
  roleEntities,
  routeParams,
  (state, { name }) => find({ name }, state)
);
