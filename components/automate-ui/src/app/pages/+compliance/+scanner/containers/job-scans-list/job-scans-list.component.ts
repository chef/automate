import { combineLatest as observableCombineLatest,  Observable, Subject } from 'rxjs';
import { map, takeUntil } from 'rxjs/operators';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from '../../../../../ngrx.reducers';
import * as moment from 'moment';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import * as selectors from '../../state/scanner.selectors';
import * as actions from '../../state/scanner.actions';

@Component({
  templateUrl: './job-scans-list.component.html',
  styleUrls: ['./job-scans-list.component.scss']
})
export class JobScansListComponent implements OnInit, OnDestroy {
  constructor(
    public route: ActivatedRoute,
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private layoutFacade: LayoutFacadeService
  ) {}

  jobScansList;

  jobDetail$: Observable<any>;

  jobDeletePrompt$: Observable<any>;

  showJobResults = false;
  openResults = {};

  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Compliance);
    this.store.select(selectors.jobScansList).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(jobScansList => this.jobScansList = jobScansList);

    this.jobDetail$ = this.store.select(selectors.jobDetail);
    this.jobDeletePrompt$ = this.store.select(selectors.jobDeletePrompt);

    observableCombineLatest([
        this.route.params,
        this.route.queryParams
      ]).pipe(
      map(([p, queryParams]) => {
        const params = Object.assign({}, queryParams);
        const {page, per_page} = params;
        if (page && page.length) {
          params['page'] = parseInt(page, 10);
        }
        if (per_page && per_page.length) {
          params['per_page'] = parseInt(per_page, 10);
        }
        return [p, params];
      }))
      .subscribe(([p, params]) => {
        const defaultParams = {
          filters: [
            {key: 'job_type', values: ['exec']},
            {key: 'parent_job', values: [p['id']]}
          ],
          page: 1,
          per_page: 100,
          sort: 'end_time',
          order: 'desc'
        };

        const jobsParams = Object.assign({}, defaultParams, params);
        this.store.dispatch(actions.getJobScans(jobsParams));
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
    const {sort, order} = this.jobScansList;
    if (sortKey === sort) {
      return order;
    }
    return 'none';
  }

  // TODO we should create some pipes for some of our common
  // date formatting needs.
  timeFromNow(time) {
    // `null` dates from API are currently sent as "beginning of time"
    if (moment(time).isBefore('2000-01-01T00:00:00.000Z')) { return '-'; }
    return moment(time).fromNow();
  }

  viewReport(jobID) {
    const filters = 'job_id:' + jobID;
    this.router.navigate(['/compliance', 'reports', 'overview'], {queryParams: {filters}});
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

  isOpenResult({ node_id }) {
    return this.openResults[node_id];
  }

  toggleResult({ node_id }) {
    this.openResults[node_id] = !this.openResults[node_id];
  }
}
