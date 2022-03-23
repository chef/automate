import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { OrgUsersActionTypes, OrgUsersActions } from './org-users.action';
import { OrgUser } from './org-users.model';

export interface OrgUserEntityState extends EntityState<OrgUser> {
  getAllStatus: EntityStatus;
  orgUserList: {
    orgUsers: OrgUser[]
  };
}

const GET_ALL_STATUS = 'getAllStatus';

export const orgUserEntityAdapter: EntityAdapter<OrgUser> =
  createEntityAdapter<OrgUser>({
    selectId: (orgUser: OrgUser) => orgUser.user_id
});

export const OrgUserEntityInitialState: OrgUserEntityState =
  orgUserEntityAdapter.getInitialState(<OrgUserEntityState>{
    getAllStatus: EntityStatus.notLoaded
  });

export function orgUserEntityReducer(
  state: OrgUserEntityState = OrgUserEntityInitialState,
  action: OrgUsersActions): OrgUserEntityState {

  switch (action.type) {
    case OrgUsersActionTypes.GET_ALL:
      return set(GET_ALL_STATUS, EntityStatus.loading, orgUserEntityAdapter.removeAll(state));

    case OrgUsersActionTypes.GET_ALL_SUCCESS:
      return pipe(
        set(GET_ALL_STATUS, EntityStatus.loadingSuccess),
        set('orgUserList.items', action.payload.orgUsers || [])
      )(state) as OrgUserEntityState;

    case OrgUsersActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: OrgUserEntityState) => state.entities[id];
