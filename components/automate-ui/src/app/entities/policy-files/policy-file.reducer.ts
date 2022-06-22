import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { PolicyFileActionTypes, PolicyFileActions } from './policy-file.action';
import { PolicyFile } from './policy-file.model';

export interface PolicyFileEntityState extends EntityState<PolicyFile> {
  getAllStatus: EntityStatus;
  deleteStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';
const DELETE_STATUS  = 'deleteStatus';

export const policyFileEntityAdapter: EntityAdapter<PolicyFile> =
  createEntityAdapter<PolicyFile>({
  selectId: (policyFile: PolicyFile) => policyFile.revision_id
});

export const PolicyFileEntityInitialState: PolicyFileEntityState =
  policyFileEntityAdapter.getInitialState(<PolicyFileEntityState>{
  getAllStatus: EntityStatus.notLoaded,
  deleteStatus: EntityStatus.notLoaded
});

export function policyFileEntityReducer(
  state: PolicyFileEntityState = PolicyFileEntityInitialState,
  action: PolicyFileActions): PolicyFileEntityState {

  switch (action.type) {
    case PolicyFileActionTypes.GET_ALL:
      return set(GET_ALL_STATUS, EntityStatus.loading, policyFileEntityAdapter.removeAll(state));

    case PolicyFileActionTypes.GET_ALL_SUCCESS:
      return pipe(
        set(GET_ALL_STATUS, EntityStatus.loadingSuccess)
        )
        (policyFileEntityAdapter.setAll(action.payload.policies, state)) as
        PolicyFileEntityState;

    case PolicyFileActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    case PolicyFileActionTypes.DELETE:
      return set(DELETE_STATUS, EntityStatus.loading, state);

    case PolicyFileActionTypes.DELETE_SUCCESS:
      return pipe(
        set(DELETE_STATUS, EntityStatus.loadingSuccess)
      )(state) as PolicyFileEntityState;

    case PolicyFileActionTypes.DELETE_FAILURE:
      return set(DELETE_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (revision_id: string) =>
  (state: PolicyFileEntityState) => state.entities[revision_id];
