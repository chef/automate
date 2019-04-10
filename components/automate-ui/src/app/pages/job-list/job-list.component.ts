import { Component } from '@angular/core';
import { Observable } from 'rxjs';
import { Store } from '@ngrx/store';
import { get, toUpper, pipe, set, pick } from 'lodash/fp';

import { NgrxStateAtom } from '../../ngrx.reducers';

import { Job } from '../../entities/jobs/job.model';
import { allJobs, jobStatus } from '../../entities/jobs/job.selectors';
import { GetJobs } from '../../entities/jobs/job.actions';
import { EntityStatus } from '../../entities/entities';
import { jobListState } from './job-list.selectors';
import { SortJobList } from './job-list.actions';
import { JobOrder } from './job-list.reducer';

@Component({
  templateUrl: './job-list.component.html',
  styleUrls: ['./job-list.component.scss']
})
export class JobListComponent {
  public jobs$: Observable<Job[]>;
  public jobStatus$: Observable<EntityStatus>;
  public sortBy: string;
  public orderBy: JobOrder;

  constructor(
    private store: Store<NgrxStateAtom>
  ) {
    this.jobs$ = store.select(allJobs);
    this.jobStatus$ = store.select(jobStatus);

    store.select(jobListState).subscribe((state) => {
      let params: object = pick(['filters', 'page', 'per_page'], state);

      this.sortBy = get('sort', state);
      this.orderBy = get('order', state);

      if (this.orderBy !== 'none') {
        params = pipe(
          set('order', toUpper(this.orderBy)),
          set('sort', this.sortBy)
        )(params);
      }

      this.store.dispatch(new GetJobs(params));
    });

  }

  loading(status) {
    return status === 'loading';
  }

  orderFor(column) {
    return column === this.sortBy ? this.orderBy : 'none';
  }

  handleSortToggle({detail: sortParams}) {
    this.store.dispatch(new SortJobList(sortParams));
  }

}
