import { createFeatureSelector, createSelector } from "@ngrx/store";
import { ssoConfigEntityAdapter, SsoConfigEntityState } from "./sso-config.reducer";

export const ssoConfigState =
  createFeatureSelector<SsoConfigEntityState>('ssoConfig');

export const { 
  selectAll: allSsoConfigs, 
  selectEntities: ssoConfigEntities
} = ssoConfigEntityAdapter.getSelectors(ssoConfigState);

export const getStatus = createSelector(
  ssoConfigState,
  (state) => state.getStatus
);

export const ssoConfig = createSelector(
  ssoConfigState,
  (state) => state.ssoConfig
);

export const saveStatus = createSelector(
  ssoConfigState,
  (state) => state.saveStatus
);

export const saveError = createSelector(
  ssoConfigState,
  (state) => state.saveError
);

export const deleteStatus = createSelector(
  ssoConfigState,
  (state) => state.deleteStatus
);
