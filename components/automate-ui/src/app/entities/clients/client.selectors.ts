import { createSelector, createFeatureSelector } from '@ngrx/store';
import { ClientEntityState, clientEntityAdapter } from './client.reducer';

export const clientState = createFeatureSelector<ClientEntityState>('clients');
export const {
  selectAll: allClients,
  selectEntities: clientEntities
} = clientEntityAdapter.getSelectors(clientState);

export const clientStatus = createSelector(
  clientState,
  (state) => state.clientsStatus
);

export const getAllStatus = createSelector(
  clientState,
  (state) => state.getAllStatus
);

export const getSearchStatus = createSelector(
  clientState,
  (state) => state.getSearchStatus
);
