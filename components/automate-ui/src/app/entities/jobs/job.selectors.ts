import { createSelector, createFeatureSelector } from '@ngrx/store';

import { JobEntityState, jobEntityAdapter } from './job.reducer';


export const jobState = createFeatureSelector<JobEntityState>('jobs');

export const {
  selectIds: jobIds,
  selectEntities: jobEntities,
  selectAll: allJobs,
  selectTotal: totalJobs
} = jobEntityAdapter.getSelectors(jobState);

export const jobStatus = createSelector(
  jobState,
  (state) => state.status
);
