import { createSelector, createFeatureSelector } from '@ngrx/store';
import { ControlDetailsEntityState, controlDetailsEntityAdapter } from './control-details.reducer';

export const ControlDetailsState = createFeatureSelector<ControlDetailsEntityState>('controlDetails');
export const {
  selectAll: allControlDetails,
  selectEntities: controlDetailsEntities
} = controlDetailsEntityAdapter.getSelectors(ControlDetailsState);

export const getStatus = createSelector(
  ControlDetailsState,
  (state) => state.ControlDetailsState
);
