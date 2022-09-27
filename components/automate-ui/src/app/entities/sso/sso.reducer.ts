import { EntityState, EntityAdapter, createEntityAdapter } from "@ngrx/entity";
import { HttpErrorResponse } from "@angular/common/http";

import { Sso } from "./sso.model";
import { set } from "lodash/fp";
import { SsoActionTypes, SsoActions } from "./sso.actions";
import { EntityStatus } from "../entities";

export interface SsoEntityState extends EntityState<Sso> {
  status: EntityStatus;
  saveStatus: EntityStatus;
  saveError: HttpErrorResponse;
}

const STATUS = "status";

export const ssoEntityAdapter: EntityAdapter<Sso> = 
  createEntityAdapter<Sso>();

export const SsoEntityInitialState: SsoEntityState =
  ssoEntityAdapter.getInitialState({
    status: EntityStatus.notLoaded,
    saveStatus: EntityStatus.notLoaded,
    saveError: null
  });

export function SsoEntityReducer(
  state: SsoEntityState = SsoEntityInitialState,
  action: SsoActions
): SsoEntityState {
  switch (action.type) {
    case SsoActionTypes.GET_ALL: {
      return set(STATUS, EntityStatus.loading, state);
    }

    case SsoActionTypes.GET_ALL_SUCCESS: {
      return set(
        STATUS,
        EntityStatus.loadingSuccess,
        ssoEntityAdapter.setAll(action.payload.sso, state)
      ) as SsoEntityState;
    }

    case SsoActionTypes.GET_ALL_FAILURE: {
      return set(STATUS, EntityStatus.loadingFailure, state 
      )as SsoEntityState;
    }
  }
  return state;
}
