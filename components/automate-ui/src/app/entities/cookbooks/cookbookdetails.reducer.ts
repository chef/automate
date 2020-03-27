import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { CookbookDetailsActionTypes, CookbookDetailsActions } from './cookbookdetails.actions';
import { CookbookDetails } from './cookbookdetails.model';

export interface CookbookDetailsEntityState extends EntityState<CookbookDetails> {
  status: EntityStatus;
  getStatus: EntityStatus;
}

const GET_STATUS = 'getStatus';

export const cookbookDetailsEntityAdapter: EntityAdapter<CookbookDetails> = createEntityAdapter<CookbookDetails>({
  selectId: (cookbookdetails: CookbookDetails) => cookbookdetails.name
});

export const CookbookDetailsEntityInitialState: CookbookDetailsEntityState =
  cookbookDetailsEntityAdapter.getInitialState(<CookbookDetailsEntityState>{
    getStatus: EntityStatus.notLoaded
  });

export function cookbookDetailsEntityReducer(
  state: CookbookDetailsEntityState = CookbookDetailsEntityInitialState,
  action: CookbookDetailsActions): CookbookDetailsEntityState {

  switch (action.type) {
    case CookbookDetailsActionTypes.GET:
      return set(GET_STATUS, EntityStatus.loading, cookbookDetailsEntityAdapter.removeAll(state));

    case CookbookDetailsActionTypes.GET_SUCCESS:
      return set(GET_STATUS, EntityStatus.loadingSuccess, 
        cookbookDetailsEntityAdapter.addOne(action.payload, state));

    case CookbookDetailsActionTypes.GET_FAILURE:
      return set(GET_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (name: string) => 
(state: CookbookDetailsEntityState) => state.entities[name];