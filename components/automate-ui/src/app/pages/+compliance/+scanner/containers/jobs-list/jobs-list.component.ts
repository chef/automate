import { timer as observableTimer,  Observable, Subject } from 'rxjs';

import { withLatestFrom, takeUntil } from 'rxjs/operators';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from '../../../../../ngrx.reducers';
import * as moment from 'moment';
import * as selectors from '../../state/scanner.selectors';
import * as actions from '../../state/scanner.actions';

@Component({
  templateUrl: './jobs-list.component.html',
  styleUrls: ['./jobs-list.component.scss']
})
export class JobsListComponent implements OnInit, OnDestroy {
  constructor(
    private store: Store<NgrxStateAtom>,
    private route: ActivatedRoute,
    private router: Router
  ) {}

  jobsList;

  jobDetail$: Observable<any>;

  jobDeletePrompt$: Observable<any>;

  showJobResults = false;
  openResults = {};

  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  ngOnInit(): void {
    this.store.select(selectors.jobsList).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(jobsList => this.jobsList = jobsList);

    this.jobDetail$ = this.store.select(selectors.jobDetail);
    this.jobDeletePrompt$ = this.store.select(selectors.jobDeletePrompt);

    // poll for updated jobs data every 5 secs (arbitrary interval)
    observableTimer(0, 5000).pipe(
      withLatestFrom(this.store.select(selectors.jobsListParams)),
      takeUntil(this.isDestroyed))
      .subscribe(([_i, params]) => {
        this.store.dispatch(actions.getJobs(params));
      });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  onPageChanged(page) {
    this.router.navigate([], {queryParams: {page}, queryParamsHandling: 'merge'});
  }

  onSortToggled(event) {
    let {sort, order} = event.detail;
    if (order === 'none') {
      sort = undefined;
      order = undefined;
    }

    const {page, per_page} = this.route.snapshot.queryParams;
    const queryParams = {sort, order, page, per_page};

    this.router.navigate([], {queryParams});
  }

  onJobResultsShow(_event, id) {
    this.showJobResults = true;
    this.store.dispatch(actions.getJob(id));
  }

  onJobResultsHide(_event) {
    this.showJobResults = false;
    this.openResults = {};
  }

  trackBy(index, _item) {
    return index;
  }

  displayNodeCount(nodeCount, recurrence) {
    // only show nodecount on child and one-off jobs
    if (recurrence === '') {
      return nodeCount;
    }
    return '-';
  }

  orderFor(sortKey) {
    const {sort, order} = this.jobsList;
    if (sortKey === sort) {
      return order;
    }
    return 'none';
  }

  isJobReport(job) {
    return job.recurrence.length === 0;
  }

  // TODO we should create some pipes for some of our common
  // date formatting needs.
  timeFromNow(time) {
    // `null` dates from API are currently sent as "beginning of time"
    if (moment(time).isBefore('2000-01-01T00:00:00.000Z')) { return '-'; }
    return moment(time).fromNow();
  }

  viewReport(jobID: string, endDate) {
    // `null` dates from API are currently sent as "beginning of time"
    if (!endDate || moment(endDate).isBefore('2000-01-01T00:00:00.000Z')) {
      this.router.navigate(['/compliance', 'reports', 'overview'],
        {queryParams: {job_id: jobID}});
    } else {
      const endDateString = moment(endDate).format('YYYY-MM-DD');

      this.router.navigate(['/compliance', 'reports', 'overview'],
        {queryParams: {job_id: jobID, end_time: endDateString}});
    }
  }

  promptDeleteJob(job) {
    this.store.dispatch(actions.promptDeleteJob(job));
  }

  confirmDeleteJob(job) {
    this.store.dispatch(actions.confirmDeleteJob(job));
  }

  cancelDeleteJob(job) {
    this.store.dispatch(actions.cancelDeleteJob(job));
  }

  statusIcon(status) {
    switch (status) {
      case ('failed'): return 'report_problem';
      case ('passed'): return 'check_circle';
      case ('skipped'): return 'help';
      default: return '';
    }
  }

  isOpenResult({ node_id }) {
    return this.openResults[node_id];
  }

  toggleResult({ node_id }) {
    this.openResults[node_id] = !this.openResults[node_id];
  }
}
