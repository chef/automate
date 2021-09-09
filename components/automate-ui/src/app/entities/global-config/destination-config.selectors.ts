import { createSelector, createFeatureSelector } from '@ngrx/store';


 import {
  GlobalConfigEntityState
 } from './destination-config.reducer';


export const globalDataFeedConfigState = createFeatureSelector<GlobalConfigEntityState>('globalDataFeedConfig');


export const globalDataFeedStatus = createSelector(
  globalDataFeedConfigState,
  (state) => state.globalConfigStatus
);

export const globalDataFeed = createSelector(
  globalDataFeedConfigState,
  (state) => {
    return {
      globalConfig: state.globalConfig,
      globalConfigStatus: state.globalConfigStatus
    };
  }
);
