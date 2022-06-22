import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { PolicyFileActionTypes, PolicyFileActions } from './policy-file.action';
import { PolicyGroup } from './policy-file.model';

export interface PolicyGroupDetailsEntityState extends EntityState<PolicyGroup> {
  policyGroupStatus: any;
  getStatus: EntityStatus;
}

const GET_STATUS = 'getStatus';

export const policyGroupDetailsEntityAdapter: EntityAdapter<PolicyGroup> =
  createEntityAdapter<PolicyGroup>({
  selectId: (policyGroup: PolicyGroup) => policyGroup.name
});

export const PolicyFileEntityInitialState: PolicyGroupDetailsEntityState =
  policyGroupDetailsEntityAdapter.getInitialState(<PolicyGroupDetailsEntityState>{
    getStatus: EntityStatus.notLoaded
});

export function policyGroupDetailsEntityReducer(
  state: PolicyGroupDetailsEntityState = PolicyFileEntityInitialState,
  action: PolicyFileActions): PolicyGroupDetailsEntityState {

  switch (action.type) {
    case PolicyFileActionTypes.GET_GROUP:
      return set(
        GET_STATUS,
        EntityStatus.loading,
        policyGroupDetailsEntityAdapter.removeAll(state)
      ) as PolicyGroupDetailsEntityState;

    case PolicyFileActionTypes.GET_GROUP_SUCCESS:
      return set(GET_STATUS, EntityStatus.loadingSuccess,
        policyGroupDetailsEntityAdapter.addOne(action.payload, state));

    case PolicyFileActionTypes.GET_GROUP_FAILURE:
      return set(GET_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}
