import { createSelector, createFeatureSelector } from '@ngrx/store';
import { InfraRoleEntityState, infraRoleEntityAdapter } from './infra-role.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const infraRoleState = createFeatureSelector<InfraRoleEntityState>('infraroles');
export const {
  selectAll: allInfraRoles,
  selectEntities: roleEntities
} = infraRoleEntityAdapter.getSelectors(infraRoleState);

export const infraRoleStatus = createSelector(
  infraRoleState,
  (state) => state.status
);

export const getAllStatus = createSelector(
  infraRoleState,
  (state) => state.getAllStatus
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
