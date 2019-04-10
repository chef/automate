import { createSelector, createFeatureSelector } from '@ngrx/store';
import { get, pipe } from 'lodash/fp';

import { NgrxStateAtom } from '../../ngrx.reducers';
import { JobAddState } from './job-add.reducer';

export const jobAddState = createFeatureSelector<JobAddState>('job_add');

export const jobAddStatus = createSelector(
  jobAddState,
  state => state.status
);

export const jobAddStep: (state: NgrxStateAtom) =>
  string = pipe(
    get(['router', 'state', 'fragment']) as any
  );
