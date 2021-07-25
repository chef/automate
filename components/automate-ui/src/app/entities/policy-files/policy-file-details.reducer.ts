import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { PolicyFileActionTypes, PolicyFileActions } from './policy-file.action';
import { PolicyFile } from './policy-file.model';

export interface PolicyFileDetailsEntityState extends EntityState<PolicyFile> {
  policyFileStatus: EntityStatus;
  getStatus: EntityStatus;
}

const GET_STATUS = 'getStatus';

export const policyFileDetailsEntityAdapter: EntityAdapter<PolicyFile> =
createEntityAdapter<PolicyFile>({
  selectId: (policyFile: PolicyFile) => policyFile.name
});

export const PolicyFileEntityInitialState: PolicyFileDetailsEntityState =
  policyFileDetailsEntityAdapter.getInitialState(<PolicyFileDetailsEntityState>{
    getStatus: EntityStatus.notLoaded
  });

export function policyFileDetailsEntityReducer(
  state: PolicyFileDetailsEntityState = PolicyFileEntityInitialState,
  action: PolicyFileActions): PolicyFileDetailsEntityState {

  switch (action.type) {
    case PolicyFileActionTypes.GET:
      return set(
        GET_STATUS,
        EntityStatus.loading,
        policyFileDetailsEntityAdapter.removeAll(state)
      ) as PolicyFileDetailsEntityState;

    case PolicyFileActionTypes.GET_SUCCESS:
      return set(GET_STATUS, EntityStatus.loadingSuccess,
        policyFileDetailsEntityAdapter.addOne(action.payload, state));

    case PolicyFileActionTypes.GET_FAILURE:
      return set(GET_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}
