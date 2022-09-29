import { createFeatureSelector, createSelector } from "@ngrx/store";
import { ssoEntityAdapter, SsoEntityState } from "./sso-config.reducer";
import { routeParams } from "app/route.selectors";
import { find } from "lodash/fp";
export const ssoConfigState =
  createFeatureSelector<SsoEntityState>('ssoConfigs');

export const { selectAll: allSsoConfigs, selectEntities: ssoConfigEntities } =
  ssoEntityAdapter.getSelectors(ssoConfigState);

export const getAllStatus = createSelector(
  ssoConfigState,
  (state) => state.getAllStatus 
);
export const ssoConfigFromRoute = createSelector(
  ssoConfigEntities,
  routeParams,
  (state, { sso_url }) => find({ sso_url }, state)
);
