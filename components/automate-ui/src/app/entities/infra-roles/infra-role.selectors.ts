import { createSelector, createFeatureSelector } from '@ngrx/store';
import { InfraRoleEntityState, infraRoleEntityAdapter } from './infra-role.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const infraRoleState = createFeatureSelector<InfraRoleEntityState>('infraRoles');
export const {
  selectAll: allInfraRoles,
  selectEntities: roleEntities
} = infraRoleEntityAdapter.getSelectors(infraRoleState);

export const infraRoleStatus = createSelector(
  infraRoleState,
  (state) => state.rolesStatus
);

export const getAllStatus = createSelector(
  infraRoleState,
  (state) => state.getAllStatus
);

export const deleteStatus = createSelector(
  infraRoleState,
  (state) => state.deleteStatus
);

export const saveStatus = createSelector(
  infraRoleState,
  (state) => state.saveStatus
);

export const saveError = createSelector(
  infraRoleState,
  (state) => state.saveError
);

export const infraRoleFromRoute = createSelector(
  roleEntities,
  routeParams,
  (state, { name }) => find({ name }, state)
);

export const roleList = createSelector(
  infraRoleState,
  (state) => state.roleList
);
