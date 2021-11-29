import { createSelector, createFeatureSelector } from '@ngrx/store';
import { ControlDetailsEntityState
  // , controlDetailsEntityAdapter 
} from './control-details.reducer';

export const ControlDetailsState = createFeatureSelector<ControlDetailsEntityState>('controlDetails');
// export const {
//   selectAll: allControlDetails
// } = controlDetailsEntityAdapter.getSelectors(ControlDetailsState);

export const controlDetailsStatus = createSelector(
  ControlDetailsState,
  (state) => state.controlDetailsStatus
);

export const controlDetailsList = createSelector(
  ControlDetailsState,
  (state) => state.controlDetailsList
);
