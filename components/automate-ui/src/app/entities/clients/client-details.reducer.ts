import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { ClientActionTypes, ClientActions } from './client.action';
import { Client, ClientKey } from './client.model';

export interface ClientDetailsEntityState extends EntityState<Client> {
  clientStatus: EntityStatus;
  getStatus: EntityStatus;
  resetKeyClient: {
    client_key: ClientKey,
    name: string
  };
  saveError: EntityStatus;
}

const GET_STATUS = 'getStatus';
const SAVE_ERROR = 'saveError';

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
      return set(
        GET_STATUS,
        EntityStatus.loadingSuccess,
        clientDetailsEntityAdapter.addOne(action.payload, state)
      ) as ClientDetailsEntityState;

    case ClientActionTypes.GET_FAILURE:
      return set(
        GET_STATUS,
        EntityStatus.loadingFailure,
        state
      ) as ClientDetailsEntityState;

    case ClientActionTypes.RESETKEY:
      return set(
        GET_STATUS,
        EntityStatus.loading,
        clientDetailsEntityAdapter.removeAll(state)
      );

    case ClientActionTypes.RESETKEY_SUCCESS: {
      return pipe (
        set( GET_STATUS, EntityStatus.loadingSuccess),
        set('resetKeyClient', action.payload || [])
        ) (state) as ClientDetailsEntityState;
      }

    case ClientActionTypes.RESETKEY_FAILURE:
      return pipe(
        set(GET_STATUS, EntityStatus.loadingFailure),
        set(SAVE_ERROR, action.payload.error)
        )( state ) as ClientDetailsEntityState;

    default:
      return state;
  }
}
