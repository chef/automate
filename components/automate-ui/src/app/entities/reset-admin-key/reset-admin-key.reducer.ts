import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { AdminKeyActionTypes, AdminKeyActions } from './reset-admin-key.actions';
import { AdminKey } from './reset-admin-key.model';

export interface AdminKeyEntityState extends EntityState<AdminKey> {
  updateStatus: EntityStatus;
}

const UPDATE_STATUS = 'updateStatus';

export const adminKeyEntityAdapter: EntityAdapter<AdminKey> = createEntityAdapter<AdminKey>();

export const AdminKeyEntityInitialState: AdminKeyEntityState =
  adminKeyEntityAdapter.getInitialState(<AdminKeyEntityState>{
    updateStatus: EntityStatus.notLoaded
  });

export function adminKeyEntityReducer(
  state: AdminKeyEntityState = AdminKeyEntityInitialState,
  action: AdminKeyActions): AdminKeyEntityState {

  switch (action.type) {
    case AdminKeyActionTypes.UPDATE:
      return set(UPDATE_STATUS, EntityStatus.loading, state);

    case AdminKeyActionTypes.UPDATE_SUCCESS:
      return set(UPDATE_STATUS, EntityStatus.loadingSuccess,
        adminKeyEntityAdapter.updateOne({
          id: action.payload.org.name,
          changes: action.payload
        }, state));

    case AdminKeyActionTypes.UPDATE_FAILURE:
      return set(UPDATE_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}
