import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';

import { Profile } from './profile.model';
import { ProfileActionTypes, ProfileActions } from './profile.actions';
import { EntityStatus } from '../entities';

export interface ProfileEntityState extends EntityState<Profile> {
  status: EntityStatus;
}

export const profileEntityAdapter: EntityAdapter<Profile> = createEntityAdapter<Profile>();

export const ProfileEntityInitialState: ProfileEntityState = profileEntityAdapter.getInitialState({
  status: EntityStatus.notLoaded
});

export function profileEntityReducer(state: ProfileEntityState = ProfileEntityInitialState,
                                     action: ProfileActions) {

  switch (action.type) {

    case ProfileActionTypes.PROFILES_SEARCH:
      return set('status', EntityStatus.loading, state);

    case ProfileActionTypes.PROFILES_SEARCH_SUCCESS:
      return set('status', EntityStatus.loadingSuccess,
                 profileEntityAdapter.addAll(action.payload.profiles, state));

    case ProfileActionTypes.PROFILES_SEARCH_FAILURE:
      return set('status', EntityStatus.loadingFailure, state);

    default:
      return state;

  }
}
