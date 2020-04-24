import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { ClientActionTypes, ClientActions } from './client.action';
import { Client } from './client.model';

export interface ClientDetailsEntityState extends EntityState<Client> {
  clientStatus: EntityStatus;
  getStatus: EntityStatus;
}

const GET_STATUS = 'getStatus';

export const clientDetailsEntityAdapter: EntityAdapter<Client> =
createEntityAdapter<Client>({
  selectId: (client: Client) => client.name
});

export const ClientEntityInitialState: ClientDetailsEntityState =
  clientDetailsEntityAdapter.getInitialState(<ClientDetailsEntityState>{
  getStatus: EntityStatus.notLoaded
});

export function clientDetailsEntityReducer(
  state: ClientDetailsEntityState = ClientEntityInitialState,
  action: ClientActions): ClientDetailsEntityState {

  switch (action.type) {
    case ClientActionTypes.GET:
      return set(
        GET_STATUS,
        EntityStatus.loading,
        clientDetailsEntityAdapter.removeAll(state)
      ) as ClientDetailsEntityState;

    case ClientActionTypes.GET_SUCCESS:
      return set(GET_STATUS, EntityStatus.loadingSuccess,
        clientDetailsEntityAdapter.addOne(action.payload, state));
    case ClientActionTypes.GET_FAILURE:
      return set(GET_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}
