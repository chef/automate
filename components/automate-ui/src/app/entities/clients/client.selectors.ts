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

export const deleteStatus = createSelector(
  clientState,
  (state) => state.deleteStatus
);

export const clientList = createSelector(
  clientState,
  (state) => state.clientList
);

export const saveStatus = createSelector(
  clientState,
  (state) => state.saveStatus
);

export const saveError = createSelector(
  clientState,
  (state) => state.saveError
);

export const createClient = createSelector(
  clientState,
  (state) => state.createClient
);
