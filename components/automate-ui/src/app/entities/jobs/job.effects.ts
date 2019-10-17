import { map, mergeMap } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  JobActionTypes,
  JobActions,
  GetJobsSuccess,
  JobGetSuccess,
  JobCreateSuccess,
  JobUpdateSuccess
} from './job.actions';
import { JobRequests } from './job.requests';

@Injectable()
export class JobEffects {
  constructor(
    private actions$: Actions,
    private requests: JobRequests
  ) {}

  @Effect()
  fetchJobs$ = this.actions$.pipe(
    ofType(JobActionTypes.GET_JOBS),
    mergeMap((action: JobActions) => this.requests.fetchJobs(action.payload)),
    map(({ jobs }) => new GetJobsSuccess({ jobs })));

  @Effect()
  jobGet$ = this.actions$.pipe(
    ofType(JobActionTypes.JOB_GET),
    mergeMap((action: JobActions) => this.requests.jobGet(action.payload)),
    map((payload) => new JobGetSuccess(payload)));

  @Effect()
  jobCreate$ = this.actions$.pipe(
    ofType(JobActionTypes.JOB_CREATE),
    mergeMap((action: JobActions) => this.requests.jobCreate(action.payload)),
    map((payload: { id: string, name: string }) => new JobCreateSuccess(payload)));

  @Effect()
  jobCreateSuccess$ = this.actions$.pipe(
    ofType<JobCreateSuccess>(JobActionTypes.JOB_CREATE_SUCCESS),
    map((payload) => new CreateNotification({
      type: Type.info,
      message: `Created a scan job ${payload.payload.name}.`
    })));

  @Effect()
  jobUpdate$ = this.actions$.pipe(
    ofType(JobActionTypes.JOB_UPDATE),
    mergeMap((action: JobActions) => this.requests.jobUpdate(action.payload)),
    map(payload => new JobUpdateSuccess(payload)));

  @Effect()
  jobUpdateSuccess$ = this.actions$.pipe(
    ofType(JobActionTypes.JOB_UPDATE_SUCCESS),
    map(() => new CreateNotification({
      type: Type.info,
      message: 'Updated a scan job.'
    })));
}
