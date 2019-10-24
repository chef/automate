import { withLatestFrom, switchMap, mergeMap, filter, map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { Store } from '@ngrx/store';
import { ROUTER_NAVIGATION, RouterNavigationAction } from '@ngrx/router-store';
import { pick } from 'lodash';
import { environment as env } from '../../../../../environments/environment';
import { ScannerState } from './scanner.state';
import * as actions from './scanner.actions';
import * as selectors from './scanner.selectors';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

@Injectable()
export class ScannerEffects {
  constructor(
    private actions$: Actions,
    private store: Store<ScannerState>,
    private httpClient: HttpClient
  ) {}

  @Effect()
  navToScanner$ = this.actions$.pipe(
    ofType(ROUTER_NAVIGATION),
    map((action: RouterNavigationAction) => action.payload.routerState),
    filter(routerState => {
      const path = routerState.url.split('?')[0];
      return path === '/compliance/scan-jobs/jobs' ||
             path === '/compliance/scan-jobs/nodes';
    }),
    map((routerState: any) => {
      const params = Object.assign({}, routerState.queryParams);
      const {page, per_page} = params;
      if (page && page.length) {
        params['page'] = parseInt(page, 10);
      }
      if (per_page && per_page.length) {
        params['per_page'] = parseInt(per_page, 10);
      }
      return [routerState.url, params];
    }),
    mergeMap(([path, params]) => {
      const nodesQueryParams = path.startsWith('/compliance/scan-jobs/nodes') ? params : {};
      const nodesStatusFilter = { key: 'status', values: ['reachable', 'unreachable', 'unknown'] };
      // ensures that we only collect nodes that belong to a manager
      // (Automate (manually managed) nodes + nodes from cloud integrations)
      const nodesManagersFilter = { key: 'manager_type', values: [''], exclude: true };
      const defaultNodesParams = {
        filters: [
          nodesStatusFilter,
          nodesManagersFilter
        ],
        page: 1,
        per_page: 100
      };

      if (nodesQueryParams.status) {
        nodesStatusFilter.values = [nodesQueryParams.status];
      }

      const jobsQueryParams = path.startsWith('/compliance/scan-jobs/jobs') ? params : {};
      const defaultJobsParams = {
        filters: [
          {key: 'job_type', values: ['exec']},
          {key: 'parent_job', values: ['']}
        ],
        page: 1,
        per_page: 100,
        sort: 'end_time',
        order: 'desc'
      };

      const jobsParams = Object.assign({}, defaultJobsParams, jobsQueryParams, {loading: true});
      const nodesParams = Object.assign({}, defaultNodesParams, nodesQueryParams, {loading: true});
      return [
        actions.getJobs(jobsParams),
        actions.getNodes(nodesParams)
      ];
    }));

  @Effect()
  getJobs$ = this.actions$.pipe(
    ofType(actions.GET_JOBS),
    switchMap((action: actions.ScannerAction) => {
      const params = pick(action.payload, ['filters', 'page', 'per_page', 'sort', 'order']);
      if (params['order']) { params['order'] = params['order'].toUpperCase(); }
      const url = `${env.compliance_url}/scanner/jobs/search`;
      return this.httpClient.post(url, params);
    }),
    map(actions.getJobsSuccess));

  @Effect()
  getJob$ = this.actions$.pipe(
    ofType(actions.GET_JOB),
    switchMap((action: actions.ScannerAction) => {
      const id = action.payload;
      const url = `${env.compliance_url}/scanner/jobs/id/${id}`;
      return this.httpClient.get(url);
    }),
    map(actions.getJobSuccess));

  @Effect()
  getJobScans$ = this.actions$.pipe(
    ofType(actions.GET_JOB_SCANS),
    switchMap((action: actions.ScannerAction) => {
      const params = pick(action.payload, ['filters', 'page', 'per_page', 'sort', 'order']);
      if (params['order']) { params['order'] = params['order'].toUpperCase(); }
      const url = `${env.compliance_url}/scanner/jobs/search`;
      return this.httpClient.post(url, params);
    }),
    map(actions.getJobScansSuccess));

  @Effect()
  getNodes$ = this.actions$.pipe(
    ofType(actions.GET_NODES),
    switchMap((action: actions.ScannerAction) => {
      const params = pick(action.payload, ['filters', 'page', 'per_page', 'sort', 'order']);
      if (params['order']) { params['order'] = params['order'].toUpperCase(); }
      const url = `${env.nodes_url}/search`;
      return this.httpClient.post(url, params);
    }),
    map(actions.getNodesSuccess));

  @Effect()
  getNode$ = this.actions$.pipe(
    ofType(actions.GET_NODE),
    switchMap((action: actions.ScannerAction) => {
      const id = action.payload;
      const url = `${env.nodes_url}/id/${id}`;
      return this.httpClient.get(url);
    }),
    map(actions.getNodeSuccess));

  @Effect()
  rerunNode$ = this.actions$.pipe(
    ofType(actions.RERUN_NODE),
      switchMap((action: actions.ScannerAction) => {
        const node = action.payload;
        const url = `${env.nodes_url}/rerun/id/${node.id}`;
        return this.httpClient.get(url);
      }),
      map(actions.rerunNodeSuccess)
    );

  @Effect()
  createJob$ = this.actions$.pipe(
    ofType(actions.CREATE_JOB),
    switchMap((action: actions.ScannerAction) => {
      const url = `${env.compliance_url}/scanner/jobs`;
      return this.httpClient.post(url, action.payload);
    }),
    map(actions.createJobSuccess));

  @Effect()
  updateJob$ = this.actions$.pipe(
    ofType(actions.UPDATE_JOB),
    switchMap((action: actions.ScannerAction) => {
      const job = action.payload;
      const url = `${env.compliance_url}/scanner/jobs/id/${job.id}`;
      return this.httpClient.put(url, job);
    }),
    map(actions.updateJobSuccess));

  @Effect()
  confirmDeleteJob$ = this.actions$.pipe(
    ofType(actions.CONFIRM_DELETE_JOB),
    map((action: actions.ScannerAction) => actions.deleteJob(action.payload)));

  @Effect()
  deleteJob$ = this.actions$.pipe(
    ofType(actions.DELETE_JOB),
    switchMap((action: actions.ScannerAction) => {
      const job = action.payload;
      const url = `${env.compliance_url}/scanner/jobs/id/${job.id}`;
      return this.httpClient.delete(url).pipe(map(() => job));
    }),
    map(actions.deleteJobSuccess));

  @Effect()
  deleteJobSuccess$ = this.actions$.pipe(
    ofType(actions.DELETE_JOB_SUCCESS),
    withLatestFrom(
      this.store.select(selectors.jobsListParams),
      this.store.select(selectors.jobScansListParams)
    ),
    map(([action, jobsListParams, jobScansListParams]: [actions.ScannerAction, any, any]) => {
      const job = action.payload;
      return job.parent_id === '' ?
        actions.getJobs(jobsListParams) :
        actions.getJobScans(jobScansListParams);
    }),
    map(() => new CreateNotification({
      type: Type.info,
      message: 'Deleted a scan job.'
    })));

  @Effect()
  deleteNode$ = this.actions$.pipe(
    ofType(actions.DELETE_NODE),
    switchMap((action: actions.ScannerAction) => {
      const node = action.payload;
      const url = `${env.nodes_url}/id/${node.id}`;
      return this.httpClient.delete(url);
    }),
    map(actions.deleteNodeSuccess));

  @Effect()
  deleteNodeSuccess$ = this.actions$.pipe(
    ofType(actions.DELETE_NODE_SUCCESS),
    withLatestFrom(this.store.select(selectors.nodesListParams)),
    map(([_action, params]) => params),
    map(actions.getNodes));
}
