import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';

import { Job } from './job.model';
import { JobActionTypes, JobActions } from './job.actions';
import { EntityStatus } from '../entities';

export interface JobEntityState extends EntityState<Job> {
  status: EntityStatus;
}

export const jobEntityAdapter: EntityAdapter<Job> = createEntityAdapter<Job>();

export const JobEntityInitialState: JobEntityState = jobEntityAdapter.getInitialState({
  status: EntityStatus.notLoaded
});

export function jobEntityReducer(state: JobEntityState = JobEntityInitialState,
                                 action: JobActions) {

  switch (action.type) {

    case JobActionTypes.GET_JOBS:
      return set('status', EntityStatus.loading, state);

    case JobActionTypes.GET_JOBS_SUCCESS:
      return set('status', EntityStatus.loadingSuccess,
                 jobEntityAdapter.addAll(action.payload.jobs, state));

    case JobActionTypes.GET_JOBS_FAILURE:
      return set('status', EntityStatus.loadingFailure, state);

    case JobActionTypes.JOB_GET_SUCCESS:
      const ids: any[] = state.ids;
      const id: string = action.payload.id;
      return set('status', EntityStatus.loadingSuccess, ids.includes(id) ?
        jobEntityAdapter.updateOne({ id, changes: action.payload }, state) :
        jobEntityAdapter.addOne(action.payload, state)
      );

    default:
      return state;

  }
}
