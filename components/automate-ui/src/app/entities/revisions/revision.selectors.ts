import { createSelector, createFeatureSelector } from '@ngrx/store';
import { RevisionEntityState, revisionEntityAdapter } from './revision.reducer';

export const revisionState = createFeatureSelector<RevisionEntityState>('revisions');
export const {
  selectAll: allRevisions,
  selectEntities: revisionEntities
} = revisionEntityAdapter.getSelectors(revisionState);

export const revisionStatus = createSelector(
  revisionState,
  (state) => state.revisionsStatus
);

export const getAllStatus = createSelector(
  revisionState,
  (state) => state.getAllStatus
);
