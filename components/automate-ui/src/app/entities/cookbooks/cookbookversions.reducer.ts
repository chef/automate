import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { CookbookVersionsActionTypes, CookbookVersionsActions } from './cookbookversions.actions';
import { CookbookVersions } from './cookbookversions.model';

export interface CookbookVersionsEntityState extends EntityState<CookbookVersions> {
  status: EntityStatus;
  getStatus: EntityStatus;
}

const GET_STATUS = 'getStatus';

export const cookbookVersionsEntityAdapter:
EntityAdapter<CookbookVersions> = createEntityAdapter<CookbookVersions>({
  selectId: (cookbookversions: CookbookVersions) => cookbookversions.name
});

export const CookbookVersionsEntityInitialState: CookbookVersionsEntityState =
cookbookVersionsEntityAdapter.getInitialState(<CookbookVersionsEntityState>{
    getStatus: EntityStatus.notLoaded
  });

export function cookbookVersionsEntityReducer(
  state: CookbookVersionsEntityState = CookbookVersionsEntityInitialState,
  action: CookbookVersionsActions): CookbookVersionsEntityState {

  switch (action.type) {
    case CookbookVersionsActionTypes.GET:
      return set(GET_STATUS, EntityStatus.loading, cookbookVersionsEntityAdapter.removeAll(state));

    case CookbookVersionsActionTypes.GET_SUCCESS:
      return pipe(
        set(GET_STATUS, EntityStatus.loadingSuccess))
        (cookbookVersionsEntityAdapter
          .addOne(action.payload, state)) as CookbookVersionsEntityState;

    case CookbookVersionsActionTypes.GET_FAILURE:
      return set(GET_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (name: string) =>
(state: CookbookVersionsEntityState) => state.entities[name];
