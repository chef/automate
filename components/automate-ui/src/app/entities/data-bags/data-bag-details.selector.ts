import { createSelector, createFeatureSelector } from '@ngrx/store';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';
import { DataBagItemsEntityState, dataBagItemsEntityAdapter } from './data-bag-details.reducer';


export const dataBagItemsState =
  createFeatureSelector<DataBagItemsEntityState>('dataBagItems');

export const {
  selectAll: allDataBagItems,
  selectEntities: dataBagItemsEntities
} = dataBagItemsEntityAdapter.getSelectors(dataBagItemsState);

export const getAllStatus = createSelector(
  dataBagItemsState,
  (state) => state.getAllStatus
);

export const dataBagItemsFromRoute = createSelector(
  dataBagItemsEntities,
  routeParams,
  (state, { name }) => find({ name }, state)
);

export const deleteStatus = createSelector(
  dataBagItemsState,
  (state) => state.deleteStatus
);

export const updateStatus = createSelector(
  dataBagItemsState,
  (state) => state.updateStatus
);

export const dataBagItemList = createSelector(
  dataBagItemsState,
  (state) => state.dataBagItemList
);

export const saveStatus = createSelector(
  dataBagItemsState,
  (state) => state.saveStatus
);

export const saveError = createSelector(
  dataBagItemsState,
  (state) => state.saveError
);
