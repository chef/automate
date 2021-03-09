import { createSelector, createFeatureSelector } from '@ngrx/store';
import { ClientDetailsEntityState, clientDetailsEntityAdapter } from './client-details.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const clientState =
createFeatureSelector<ClientDetailsEntityState>('clientDetails');

export const {
  selectAll: allClients,
  selectEntities: clientEntities
} = clientDetailsEntityAdapter.getSelectors(clientState);

export const clientStatus = createSelector(
  clientState,
  (state) => state.clientStatus
);

export const getStatus = createSelector(
  clientState,
  (state) => state.getStatus
);

export const clientFromRoute = createSelector(
  clientEntities,
  routeParams,
  (state, { name }) => find({ name }, state)
);

export const resetKeyClient = createSelector(
  clientState,
  (state) => state.resetKeyClient
);

export const saveError = createSelector(
  clientState,
  (state) => state.saveError
);
