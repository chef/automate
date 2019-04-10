import { set } from 'lodash/fp';
import { ROUTER_NAVIGATION, RouterAction } from '@ngrx/router-store';

import { JobActions, JobActionTypes } from '../../entities/jobs/job.actions';

export enum Status {
  notCreated = 'notCreated',
  saving = 'saving',
  success = 'success',
  failure = 'failure'
}

export interface JobAddState {
  status: Status;
}

export const JobAddInitialState: JobAddState = {
  status: Status.notCreated
};

export function jobAddReducer(state: JobAddState = JobAddInitialState,
                              action: JobActions | RouterAction<any>) {
  switch (action.type) {
    case JobActionTypes.JOB_CREATE:
      return set('status', Status.saving, state);

    case JobActionTypes.JOB_CREATE_SUCCESS:
      return set('status', Status.success, state);

    case ROUTER_NAVIGATION:
      return set('status', Status.notCreated, state);

    default:
      return state;
  }
}
