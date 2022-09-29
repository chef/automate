import { EntityState, EntityAdapter, createEntityAdapter } from "@ngrx/entity";

import { SsoConfig } from "./sso-config.model";
import { set } from "lodash/fp";
import { SsoActionTypes, SsoActions } from "./sso-config.actions";
import { EntityStatus } from "../entities";

export interface SsoEntityState extends EntityState<SsoConfig> {
  getAllStatus: EntityStatus;
  ssoConfig: SsoConfig;
}

const GET_ALL_STATUS = "getAllStatus";
const GET_STATUS = "getstatus"

export const ssoEntityAdapter: EntityAdapter<SsoConfig> = createEntityAdapter<SsoConfig>();

export const SsoEntityInitialState: SsoEntityState =
  ssoEntityAdapter.getInitialState({
    getAllStatus: EntityStatus.notLoaded,
    ssoConfig: null
  });

export function SsoEntityReducer(
  state: SsoEntityState = SsoEntityInitialState,
  action: SsoActions
): SsoEntityState {
  switch (action.type) {
    
    case SsoActionTypes.GET_ALL: {
      return set(GET_ALL_STATUS, EntityStatus.loading, state);
    }

    case SsoActionTypes.GET_ALL_SUCCESS: {
      const ssoConfigstatusState = set(
        GET_ALL_STATUS,
        EntityStatus.loadingSuccess,
        state
      );
      return set (
        GET_STATUS,
        action.payload,
        ssoConfigstatusState
      ) as SsoEntityState;
    }

    case SsoActionTypes.GET_ALL_FAILURE: {
      return set(
        GET_ALL_STATUS,
        EntityStatus.loadingFailure,
        state
      ) as SsoEntityState;
    }
    default:
      return state;
  }
}


