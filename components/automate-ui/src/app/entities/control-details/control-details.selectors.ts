import { createSelector, createFeatureSelector } from '@ngrx/store';
import { ControlDetailEntityState
  , controlDetailEntityAdapter 
} from './control-details.reducer';

export const ControlDetailsState = createFeatureSelector<ControlDetailEntityState>('controlDetails');
export const {
  selectAll: allControlDetails,
  selectEntities: controlDetailsEntities
} = controlDetailEntityAdapter.getSelectors(ControlDetailsState);

export const controlDetailStatus = createSelector(
  ControlDetailsState,
  (state) => state.controlDetailStatus
);

export const controlDetailList = createSelector(
  ControlDetailsState,
  (state) => state.controlDetailList
);

export const controlsList = createSelector(
  ControlDetailsState,
  (state) => state.controlsList
);
