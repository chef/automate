import { merge } from 'lodash/fp';

import { JobListActions, JobListActionTypes } from './job-list.actions';

export enum JobOrder {
  asc = 'asc',
  desc = 'desc',
  none = 'none'
}

export interface JobFilter {
  key: string;
  values: string[];
}

export interface JobListState {
  page: number;
  per_page: number;
  sort: string;
  order: JobOrder;
  filters: JobFilter[];
}

export const JobListInitialState: JobListState = {
  page: 1,
  per_page: 100,
  sort: 'end_time',
  order: JobOrder.desc,
  filters: [
    { key: 'job_type', values: ['exec'] },
    { key: 'parent_job', values: [''] }
  ]
};

export function jobListReducer(state: JobListState = JobListInitialState,
                               action: JobListActions) {
  switch (action.type) {

    case JobListActionTypes.SORT_JOBS:
      return merge(state, action.payload);

    default:
      return state;

  }

}
