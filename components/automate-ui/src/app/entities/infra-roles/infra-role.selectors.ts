import { createSelector, createFeatureSelector } from '@ngrx/store';
import { InfraRoleEntityState, infraRoleEntityAdapter, RoleItemEntityState , roleItemEntityAdapter} from './infra-role.reducer';
import { routeParams } from 'app/route.selectors';

export const infraRoleState = createFeatureSelector<InfraRoleEntityState>('roles');
export const roleItemState = createFeatureSelector<RoleItemEntityState>('roles');

export const {
  selectAll: allInfraRoles,
  selectEntities: roleEntities
} = infraRoleEntityAdapter.getSelectors(infraRoleState);


export const {
  selectAll: allInfraRole,
  selectEntities: roleItemEntities
} = roleItemEntityAdapter.getSelectors(roleItemState);

export const getAllStatus = createSelector(
    infraRoleState,
  (state) => state.getAllStatus
);

export const getStatus = createSelector(
    infraRoleState,
  (state) => state.getStatus
);

export const getStatusItem = createSelector(
  roleItemState,
(state) => state.getStatusItem
);

export const roleFromRoute = createSelector(
  roleEntities,
  routeParams,
  (state, { name }) => state[name]
);

export const ruleFromRoute = createSelector(
  roleItemEntities,
  routeParams,
  (state, { name }) => state[name]
);
