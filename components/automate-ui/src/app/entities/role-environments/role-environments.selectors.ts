import { createSelector, createFeatureSelector } from '@ngrx/store';
import { RoleEnvironmentEntityState, roleEnvironmentEntityAdapter } from './role-environments.reducer';

export const roleEnvironmentState = createFeatureSelector<RoleEnvironmentEntityState>('roleEnvironments');
export const {
  selectAll: allRoleEnvironments,
  selectEntities: roleEnvironmentEntities
} = roleEnvironmentEntityAdapter.getSelectors(roleEnvironmentState);

export const roleEnvironmentStatus = createSelector(
  roleEnvironmentState,
  (state) => state.roleEnvironmentsStatus
);

export const getAllStatus = createSelector(
  roleEnvironmentState,
  (state) => state.getAllStatus
);
