import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { ClientActionTypes, ClientActions } from './client.action';
import { Client } from './client.model';

export interface ClientEntityState extends EntityState<Client> {
  clientsStatus: EntityStatus;
  getAllStatus: EntityStatus;
  getSearchStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';
const GET_SEARCH_STATUS = 'getSearchStatus';

export const clientEntityAdapter: EntityAdapter<Client> =
  createEntityAdapter<Client>({
  selectId: (client: Client) => client.name
});

export const ClientEntityInitialState: ClientEntityState =
  clientEntityAdapter.getInitialState(<ClientEntityState>{
  getAllStatus: EntityStatus.notLoaded
});

export function clientEntityReducer(
  state: ClientEntityState = ClientEntityInitialState,
  action: ClientActions): ClientEntityState {

  switch (action.type) {
    case ClientActionTypes.GET_ALL:
      return set(GET_ALL_STATUS, EntityStatus.loading, clientEntityAdapter.removeAll(state));

    case ClientActionTypes.GET_ALL_SUCCESS:
      return pipe(
        set(GET_ALL_STATUS, EntityStatus.loadingSuccess))
        (clientEntityAdapter.setAll(action.payload.clients, state)) as
        ClientEntityState;

    case ClientActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    case ClientActionTypes.SEARCH:
      return set(GET_SEARCH_STATUS, EntityStatus.loading, clientEntityAdapter
        .removeAll(state));

    case ClientActionTypes.SEARCH_SUCCESS:
      return pipe(
        set(GET_SEARCH_STATUS, EntityStatus.loadingSuccess))
        (clientEntityAdapter
          .setAll(action.payload.clients, state)) as ClientEntityState;

    case ClientActionTypes.SEARCH_FAILURE:
      return set(GET_SEARCH_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: ClientEntityState) => state.entities[id];
