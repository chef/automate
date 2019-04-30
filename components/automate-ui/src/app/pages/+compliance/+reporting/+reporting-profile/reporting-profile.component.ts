import { forkJoin as observableForkJoin,
         combineLatest as observableCombineLatest,
         Subject,
         Observable } from 'rxjs';

import { map, takeUntil } from 'rxjs/operators';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { StatsService } from '../../shared/reporting/stats.service';
import { ReportQueryService } from '../../shared/reporting/report-query.service';
import { ScanResultsService } from '../../shared/reporting/scan-results.service';
import { paginationOverride } from '../shared';
import * as moment from 'moment';

@Component({
  selector: 'app-reporting-profile',
  templateUrl: './reporting-profile.component.html',
  styleUrls: [ './reporting-profile.component.scss' ]
})
export class ReportingProfileComponent implements OnInit, OnDestroy {
  profile: any = {};
  controls = [];
  displayScanResultsSidebar = false;
  showLoadingIcon = false;
  statusFilter = 'all';

  openControls: any = {};

  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    private route: ActivatedRoute,
    private statsService: StatsService,
    private reportQuery: ReportQueryService,
    public scanResults: ScanResultsService
  ) {}

  ngOnInit() {
    this.showLoadingIcon = true;

    const id = this.route.snapshot.params['id'];
    this.fetchProfile(id, this.reportQuery.filters.getValue()).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(this.onFetchedProfile.bind(this));

    observableCombineLatest([
        this.reportQuery.filters,
        this.scanResults.params
      ]).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(this.onFilterParamsChange.bind(this));
  }

  fetchProfile(id, filters) {
    const getProfile = this.statsService.getProfileResultsSummary(id, filters);
    const getControlsForProfile = this.statsService.getProfileResults(id, filters);
    return observableForkJoin([
      getProfile,
      getControlsForProfile
    ]);
  }

  onControlClick(controlId) {
    const profileId = this.route.snapshot.params['id'];
    const nodeId = null;
    this.scanResults.title = controlId;
    this.showScanResults({nodeId, profileId, controlId});
  }

  onFetchedProfile([profile, controls]) {
    this.profile = profile;
    this.controls = controls;
    this.scanResults.profile = profile;
    this.showLoadingIcon = false;
  }

  onFilterParamsChange([filters, params]) {
    const {nodeId, profileId, controlId} = params;
    if (!nodeId && profileId) {
      this.scanResults.showControlDetail = false;
      this.getNodes(filters, params).pipe(
        takeUntil(this.isDestroyed))
        .subscribe(nodes => {
          this.scanResults.showNodesList = true;
          this.scanResults.nodesList.items = nodes;
        });
    }

    if (nodeId && profileId && controlId) {
      this.scanResults.showNodesList = false;

      this.getControl(filters, params).pipe(
        takeUntil(this.isDestroyed))
        .subscribe((control) => {
          this.scanResults.control = control;
          this.scanResults.controlDetail.item = control;
          this.scanResults.showControlDetail = true;
        });
    }
  }

  onNodeSelected(node) {
    this.scanResults.node = node;
    this.scanResults.setParam('nodeId', node.id);
    this.toggleControl(node);
  }

  onNodeDeselected(_node) {
    this.scanResults.node = null;
    this.scanResults.setParam('nodeId', null);
  }

  onScanResultsClose(_event) {
    this.hideScanResults();
  }

  showScanResults(params) {
    this.scanResults.setParams(params);
    this.scanResults.opened = true;
  }

  hideScanResults() {
    this.scanResults.opened = false;
    this.scanResults.clearParams();
    this.scanResults.control = null;
    this.openControls = {};
  }

  getNodes(filters, params): Observable<Array<any>> {
    const profileFilter = {type: { name: 'profile_id' } , value: { text: params.profileId}};
    const controlFilter = {type: { name: 'control_id' } , value: { text: params.controlId}};
    filters = [profileFilter, controlFilter].concat(filters);
    params = paginationOverride;
    params['sort'] = 'latest_report.end_time';
    params['order'] = 'desc';
    return this.statsService.getNodes(filters, params).pipe(
      map(data => {
        return data.items.map(node => {
          node.status = node.latest_report.status;
          return node;
        });
      }));
  }

  getControl(filters, params): Observable<any> {
    const {node} = this.scanResults;
    const reportId = node.latest_report.id;

    return this.statsService.getSingleReport(reportId, filters).pipe(
      takeUntil(this.isDestroyed),
      map(report => {
        const {controls} = report.profiles[0];
        const control = controls.filter(c => c.id === params.controlId)[0];
        return control;
      }));
  }

  statusClass({ failed, passed }) {
    if (failed > 0) { return 'failed'; }
    if (failed === 0 && passed === 0) { return 'skipped'; }
    return 'passed';
  }

  impactStatus({ impact }) {
    if (impact >= 0.7) { return 'critical'; }
    if (impact >= 0.4) { return 'major'; }
    return 'minor';
  }

  statusIcon(status) {
    switch (status) {
      case ('failed'): return 'report_problem';
      case ('passed'): return 'check_circle';
      case ('skipped'): return 'help';
      default: return '';
    }
  }

  displaySupports(arraySupports) {
    if (arraySupports === undefined) {
      return '';
    }
    return arraySupports.map(s => {
      return [s['os_family'], s['os_name'], s['release'], s['platform']]
        .filter(v => typeof v === 'string')
        .join(' ');
    }).join('; ');
  }

  isOpenControl({ id }) {
    return this.openControls[id] && this.openControls[id].open;
  }

  toggleControl(control) {
    const state = this.openControls[control.id];
    const toggled = state ? ({...state, open: !state.open}) : ({ open: true, pane: 'results' });
    this.openControls[control.id] = toggled;
  }

  openControlPane(control) {
    return this.openControls[control.id].pane;
  }

  showControlPane(control, pane) {
    this.openControls[control.id].pane = pane;
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  formatDaysAgo(timestamp) {
    return moment(timestamp).fromNow();
  }
}
