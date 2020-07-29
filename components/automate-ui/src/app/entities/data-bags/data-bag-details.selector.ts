import { createSelector, createFeatureSelector } from '@ngrx/store';

import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

import { DataBagDetailsEntityState, dataBagDetailsEntityAdapter } from './data-bag-details.reducer';


export const dataBagDetailsState =
  createFeatureSelector<DataBagDetailsEntityState>
('dataBagDetails');

export const {
  selectAll: allDataBagDetails,
  selectEntities: dataBagDetailsEntities
} = dataBagDetailsEntityAdapter.getSelectors(dataBagDetailsState);

export const getAllStatus = createSelector(
  dataBagDetailsState,
  (state) => state.getAllStatus
);

export const dataBagDetailsFromRoute = createSelector(
  dataBagDetailsEntities,
  routeParams,
  (state, { name }) => find({ name }, state)
);
