import { createSelector, createFeatureSelector } from '@ngrx/store';
import { DataBagItemDetailsEntityState, dataBagItemDetailsEntityAdapter } from './data-bag-item-details.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const dataBagItemDetailsState = createFeatureSelector<DataBagItemDetailsEntityState>('dataBagItemDetails');
export const {
  selectAll: allDataBagItemDetails,
  selectEntities: dataBagItemDetailsEntities
} = dataBagItemDetailsEntityAdapter.getSelectors(dataBagItemDetailsState);

export const getStatus = createSelector(
  dataBagItemDetailsState,
  (state) => state.getStatus
);

export const dataBagItemDetailsFromRoute = createSelector(
  dataBagItemDetailsEntities,
  routeParams,
  (state, { name }) => find({ name }, state)
);
