import { Action } from '@ngrx/store';

import { Job } from './job.model';

export enum JobActionTypes {
  GET_JOBS = 'JOBS::GET',
  GET_JOBS_SUCCESS = 'JOBS::GET::SUCCESS',
  GET_JOBS_FAILURE = 'JOBS::GET::FAILURE',
  JOB_GET = 'JOB::GET',
  JOB_GET_SUCCESS = 'JOB::GET::SUCCESS',
  JOB_GET_FAILURE = 'JOB::GET::FAILURE',
  JOB_CREATE = 'JOB::CREATE',
  JOB_CREATE_SUCCESS = 'JOB::CREATE::SUCCESS',
  JOB_CREATE_FAILURE = 'JOB::CREATE::FAILURE',
  JOB_UPDATE = 'JOB::UPDATE',
  JOB_UPDATE_SUCCESS = 'JOB::UPDATE::SUCCESS',
  JOB_UPDATE_FAILURE = 'JOB::UPDATE::FAILURE'
}

export class GetJobs implements Action {
  readonly type = JobActionTypes.GET_JOBS;

  constructor(public payload: {}) {}
}

export class GetJobsSuccess implements Action {
  readonly type = JobActionTypes.GET_JOBS_SUCCESS;

  constructor(public payload: { jobs: Job[] }) {}
}

export class GetJobsFailure implements Action {
  readonly type = JobActionTypes.GET_JOBS_FAILURE;

  constructor(public payload: {}) {}
}

export class JobGet implements Action {
  readonly type = JobActionTypes.JOB_GET;

  constructor(public payload: {id: string}) {}
}

export class JobGetSuccess implements Action {
  readonly type = JobActionTypes.JOB_GET_SUCCESS;

  constructor(public payload: Job) {}
}

export class JobGetFailure implements Action {
  readonly type = JobActionTypes.JOB_GET_FAILURE;

  constructor(public payload: {}) {}
}

export class JobCreate implements Action {
  readonly type = JobActionTypes.JOB_CREATE;

  constructor(public payload: {}) {}
}

export class JobCreateSuccess implements Action {
  readonly type = JobActionTypes.JOB_CREATE_SUCCESS;

  constructor(public payload: { id: string, name: string }) {}
}

export class JobCreateFailure implements Action {
  readonly type = JobActionTypes.JOB_CREATE_FAILURE;

  constructor(public payload: {}) {}
}

export class JobUpdate implements Action {
  readonly type = JobActionTypes.JOB_UPDATE;

  constructor(public payload: {}) {}
}

export class JobUpdateSuccess implements Action {
  readonly type = JobActionTypes.JOB_UPDATE_SUCCESS;

  constructor(public payload: { id: string }) {}
}

export class JobUpdateFailure implements Action {
  readonly type = JobActionTypes.JOB_UPDATE_FAILURE;

  constructor(public payload: {}) {}
}

export type JobActions =
  | GetJobs
  | GetJobsSuccess
  | GetJobsFailure
  | JobGet
  | JobGetSuccess
  | JobGetFailure
  | JobCreate
  | JobCreateSuccess
  | JobCreateFailure
  | JobUpdate
  | JobUpdateSuccess
  | JobUpdateFailure;
