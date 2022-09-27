import { createFeatureSelector, createSelector } from "@ngrx/store";
import { ssoEntityAdapter, SsoEntityState } from "./sso.reducer";

export const ssoState = 
    createFeatureSelector<SsoEntityState>('sso');

export const {
    selectAll: allConfig,
  } = ssoEntityAdapter.getSelectors(ssoState);

export const saveStatus = createSelector(
    ssoState,
    (state) => state.status
)