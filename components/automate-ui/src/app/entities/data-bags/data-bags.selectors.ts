import { createSelector, createFeatureSelector } from '@ngrx/store';
import { DataBagsEntityState, dataBagsEntityAdapter } from './data-bags.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const dataBagsState = createFeatureSelector<DataBagsEntityState>('dataBags');
export const {
  selectAll: allDataBags,
  selectEntities: dataBagsEntities
} = dataBagsEntityAdapter.getSelectors(dataBagsState);

export const getAllStatus = createSelector(
  dataBagsState,
  (state) => state.getAllStatus
);

export const dataBagsFromRoute = createSelector(
  dataBagsState,
  routeParams,
  (state, { name }) => find({ name }, state)
);
