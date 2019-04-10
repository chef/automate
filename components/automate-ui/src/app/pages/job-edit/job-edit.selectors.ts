import { createSelector, createFeatureSelector } from '@ngrx/store';
import { get, pipe } from 'lodash/fp';
import { NgrxStateAtom } from '../../ngrx.reducers';
import { JobEditState } from './job-edit.reducer';

export const jobEditState = createFeatureSelector<JobEditState>('job_edit');

export const jobEditStatus = createSelector(
  jobEditState,
  state => state.status
);

export const jobEditStep: (state: NgrxStateAtom) =>
  string = pipe(
    get(['router', 'state', 'fragment']) as any
  );
