import { set } from 'lodash/fp';
import { ROUTER_NAVIGATION, RouterAction } from '@ngrx/router-store';

import { JobActions, JobActionTypes } from '../../entities/jobs/job.actions';

export enum Status {
  notUpdated = 'notUpdated',
  saving = 'saving',
  success = 'success',
  failure = 'failure'
}

export interface JobEditState {
  status: Status;
}

export const JobEditInitialState: JobEditState = {
  status: Status.notUpdated
};

export function jobEditReducer(state: JobEditState = JobEditInitialState,
                               action: JobActions | RouterAction<any>) {
  switch (action.type) {
    case JobActionTypes.JOB_UPDATE:
      return set('status', Status.saving, state);

    case JobActionTypes.JOB_UPDATE_SUCCESS:
      return set('status', Status.success, state);

    case ROUTER_NAVIGATION:
      return set('status', Status.notUpdated, state);

    default:
      return state;
  }
}
