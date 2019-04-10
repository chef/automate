import { createFeatureSelector } from '@ngrx/store';

import { JobListState } from './job-list.reducer';

export const jobListState = createFeatureSelector<JobListState>('job_list');
