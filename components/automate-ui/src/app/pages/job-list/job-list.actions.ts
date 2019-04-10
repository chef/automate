import { Action } from '@ngrx/store';

import { JobOrder } from './job-list.reducer';

export enum JobListActionTypes {
  SORT_JOBS = 'JOBS_LIST::SORT'
}

export class SortJobList implements Action {
  readonly type = JobListActionTypes.SORT_JOBS;

  constructor(public payload: { order: JobOrder, sort: string }) {}
}

export type JobListActions = SortJobList;
