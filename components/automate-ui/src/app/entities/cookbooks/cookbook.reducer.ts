import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { CookbookActionTypes, CookbookActions } from './cookbook.actions';
import { Cookbook } from './cookbook.model';

export interface CookbookEntityState extends EntityState<Cookbook> {
  getAllStatus: EntityStatus;
  getStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';

export const cookbookEntityAdapter: EntityAdapter<Cookbook> = createEntityAdapter<Cookbook>({
  selectId: (cookbook: Cookbook) => cookbook.name
});

export const CookbookEntityInitialState: CookbookEntityState =
  cookbookEntityAdapter.getInitialState(<CookbookEntityState>{
    getAllStatus: EntityStatus.notLoaded,
    getStatus: EntityStatus.notLoaded
  });

export function cookbookEntityReducer(
  state: CookbookEntityState = CookbookEntityInitialState,
  action: CookbookActions): CookbookEntityState {

  switch (action.type) {
    case CookbookActionTypes.GET_ALL:
      return set(GET_ALL_STATUS, EntityStatus.loading, cookbookEntityAdapter.removeAll(state));

    case CookbookActionTypes.GET_ALL_SUCCESS:
      return pipe(
        set(GET_ALL_STATUS, EntityStatus.loadingSuccess))
        (cookbookEntityAdapter.addAll(action.payload.cookbooks, state)) as CookbookEntityState;

    case CookbookActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (name: string) => (state: CookbookEntityState) => state.entities[name];
