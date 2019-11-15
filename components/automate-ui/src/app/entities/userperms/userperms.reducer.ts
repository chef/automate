import {
  defaults,
  keys,
  pipe,
  set,
  toPairs
} from 'lodash/fp';

import { UserPermsActions, UserPermsTypes } from './userperms.actions';
import { UserPermEntity } from './userperms.entity';
import { indexer, IndexedEntities } from '../entities';

const permIndexer = indexer(UserPermEntity.create);

export enum Status {
  notLoaded,
  loading,
  loadingSuccess,
  loadingFailure
}

export interface PermEntityState {
  readonly byId: IndexedEntities<UserPermEntity>;
  readonly allIds: string[];
  readonly status: Status;
  readonly lastTimeFetchAll: Date;
}

export const initialState: PermEntityState = {
  byId: {},
  allIds: [],
  status: Status.notLoaded,
  lastTimeFetchAll: new Date(0)
};

export function permEntityReducer(
  state: PermEntityState = initialState, action: UserPermsActions): PermEntityState {

  switch (action.type) {

    case UserPermsTypes.GET_ALL: {
      return pipe(
        set('lastTimeFetchAll', new Date()),
        set('status', Status.loading)
      )(state) as PermEntityState;
    }

    case UserPermsTypes.GET_SOME: {
      return set('status', Status.loading, state) as PermEntityState;
    }

    case UserPermsTypes.GET_ALL_SUCCESS: {
      // Data needs to be massaged slightly to feed to the indexer.
      // UserPermEntity.create will get passed a single argument (via entities.ts)
      // but we need to include the <path> along with the <perms_map>
      // so here we combine them into that single argument.
      // toPairs converts each endpoint in the payload from
      // { <path>: <perms_map> } to [<path>, <perms_map> ].
      const allPairs = toPairs(action.payload);
      // combine with what we already have so as not to erase parameterized endpoints
      // that are not included here.
      const allPerms = defaults(state.byId, permIndexer(allPairs));

      return pipe(
        set('status', Status.loadingSuccess),
        set('byId', allPerms),
        set('allIds', keys(allPerms))
      )(state) as PermEntityState;
    }

    case UserPermsTypes.GET_SOME_SUCCESS:
    case UserPermsTypes.GET_PARAMETERIZED_SUCCESS: {
      const somePairs = toPairs(action.payload);
      const somePerms = defaults(state.byId, permIndexer(somePairs));

      return pipe(
        set('status', Status.loadingSuccess),
        set('byId', somePerms),
        set('allIds', keys(somePerms))
      )(state) as PermEntityState;
    }

    case UserPermsTypes.GET_ALL_FAILURE:
    case UserPermsTypes.GET_SOME_FAILURE:
    case UserPermsTypes.GET_PARAMETERIZED_FAILURE: {
      return set('status', Status.loadingFailure, state) as PermEntityState;
    }
  }

  return state;
}
