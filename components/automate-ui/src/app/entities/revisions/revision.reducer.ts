import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { RevisionActionTypes, RevisionActions } from './revision.action';
import { Revision } from './revision.model';

export interface RevisionEntityState extends EntityState<Revision> {
  revisionsStatus: EntityStatus;
  getAllStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';

export const revisionEntityAdapter: EntityAdapter<Revision> =
  createEntityAdapter<Revision>({
   selectId: (revision: Revision) => revision.revision_id
});

export const RevisionEntityInitialState: RevisionEntityState =
  revisionEntityAdapter.getInitialState(<RevisionEntityState>{
  getAllStatus: EntityStatus.notLoaded
});

export function revisionEntityReducer(
  state: RevisionEntityState = RevisionEntityInitialState,
  action: RevisionActions): RevisionEntityState {

  switch (action.type) {
    case RevisionActionTypes.GET_ALL:
      return set(GET_ALL_STATUS, EntityStatus.loading, revisionEntityAdapter.removeAll(state));

    case RevisionActionTypes.GET_ALL_SUCCESS:
      return pipe(
        set(GET_ALL_STATUS, EntityStatus.loadingSuccess))
        (revisionEntityAdapter.setAll(action.payload.revisions, state)) as
        RevisionEntityState;

    case RevisionActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: RevisionEntityState) => state.entities[id];
