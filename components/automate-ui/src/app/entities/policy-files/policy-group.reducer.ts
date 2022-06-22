import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { PolicyFileActionTypes, PolicyFileActions } from './policy-file.action';
import { PolicyFile } from './policy-file.model';

export interface PolicyGroupEntityState extends EntityState<PolicyFile> {
  getGroupsStatus: EntityStatus;
  policyFile: PolicyFile[];
}

const GET_GROUPS_STATUS = 'getGroupsStatus';

export const policyGroupEntityAdapter: EntityAdapter<PolicyFile> =
  createEntityAdapter<PolicyFile>({
  selectId: (policyFile: PolicyFile) => policyFile.policy_group
});

export const PolicyFileEntityInitialState: PolicyGroupEntityState =
  policyGroupEntityAdapter.getInitialState(<PolicyGroupEntityState>{
    getGroupsStatus: EntityStatus.notLoaded
});

export function policyGroupsEntityReducer(
  state: PolicyGroupEntityState = PolicyFileEntityInitialState,
  action: PolicyFileActions): PolicyGroupEntityState {

    switch (action.type) {
    case PolicyFileActionTypes.GET_GROUPS:
      return set(GET_GROUPS_STATUS, EntityStatus.loading,
        policyGroupEntityAdapter.removeAll(state));

    case PolicyFileActionTypes.GET_GROUPS_SUCCESS:
      return pipe(
        set(GET_GROUPS_STATUS, EntityStatus.loadingSuccess),
        set('policyFile', action.payload.policies || [])
        )(state) as PolicyGroupEntityState;

    case PolicyFileActionTypes.GET_GROUPS_FAILURE:
      return set(GET_GROUPS_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}
