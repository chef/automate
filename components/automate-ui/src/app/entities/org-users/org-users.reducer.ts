import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { OrgUsersActionTypes, OrgUsersActions } from './org-users.action';
import { OrgUser } from './org-users.model';

export interface OrgUserEntityState extends EntityState<OrgUser> {
  getAllStatus: EntityStatus;
  orgUserList: {
    users: OrgUser[]
  };
  resetStatus: EntityStatus;
  resetError: EntityStatus;
  resetUserKey: {
    user_key: string,
    name: string
  };
}

const GET_ALL_STATUS = 'getAllStatus';
const RESET_STATUS = 'resetStatus';
const RESET_ERROR = 'resetError';

export const orgUserEntityAdapter: EntityAdapter<OrgUser> =
  createEntityAdapter<OrgUser>({
    selectId: (user: OrgUser) => user.user_id
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
        set('orgUserList.users', action.payload.users || [])
      )(state) as OrgUserEntityState;

    case OrgUsersActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    case OrgUsersActionTypes.RESETKEY:
      return set(
        RESET_STATUS,
        EntityStatus.loading,
        orgUserEntityAdapter.removeAll(state)
      );

    case OrgUsersActionTypes.RESETKEY_SUCCESS: {
      return pipe (
        set( RESET_STATUS, EntityStatus.loadingSuccess),
        set('resetUserKey', action.payload || [])
        ) (state) as OrgUserEntityState;
      }

    case OrgUsersActionTypes.RESETKEY_FAILURE:
      return pipe(
        set(RESET_STATUS, EntityStatus.loadingFailure),
        set(RESET_ERROR, action.payload.error)
        )( state ) as OrgUserEntityState;

    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: OrgUserEntityState) => state.entities[id];
