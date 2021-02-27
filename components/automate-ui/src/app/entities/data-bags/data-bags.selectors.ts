import { createSelector, createFeatureSelector } from '@ngrx/store';
import { DataBagEntityState, dataBagEntityAdapter } from './data-bags.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const dataBagState = createFeatureSelector<DataBagEntityState>('dataBag');
export const {
  selectAll: allDataBags,
  selectEntities: dataBagsEntities
} = dataBagEntityAdapter.getSelectors(dataBagState);

export const getAllStatus = createSelector(
  dataBagState,
  (state) => state.getAllStatus
);

export const deleteStatus = createSelector(
  dataBagState,
  (state) => state.deleteStatus
);

export const saveStatus = createSelector(
  dataBagState,
  (state) => state.saveStatus
);

export const saveError = createSelector(
  dataBagState,
  (state) => state.saveError
);

export const dataBagsFromRoute = createSelector(
  dataBagsEntities,
  routeParams,
  (state, { name }) => find({ name }, state)
);
