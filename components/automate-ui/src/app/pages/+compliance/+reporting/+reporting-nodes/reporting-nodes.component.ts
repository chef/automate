import { takeUntil } from 'rxjs/operators';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subject } from 'rxjs';
import { groupBy } from 'lodash';
import * as moment from 'moment';
import { Filter } from '../types';
import { paginationOverride } from '../shared';
import { StatsService, ReportQueryService, ReportDataService } from '../../shared/reporting';

@Component({
  selector: 'app-reporting-nodes',
  templateUrl: './reporting-nodes.component.html',
  styleUrls: [ './reporting-nodes.component.scss' ]
})
export class ReportingNodesComponent implements OnInit, OnDestroy {
  displayScanResultsSidebar = false;
  scanResultsNodeFilter: Filter;
  scanResultsProfileFilter: Filter;
  layerOneData: any = {};
  layerTwoData: any = {};
  control: any = {};

  scanResultsPane = 0;
  openControls = {};

  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    private statsService: StatsService,
    public reportQuery: ReportQueryService,
    public reportData: ReportDataService
  ) {}

  ngOnInit() {
    this.reportQuery.filters.pipe(
      takeUntil(this.isDestroyed))
      .subscribe(this.getData.bind(this));
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  onNodesListPageChanged(event) {
    const filters = this.reportQuery.filters.getValue();
    this.reportData.nodesListParams.page = event;
    this.getData(filters);
  }

  onNodesListSortToggled(event) {
    const filters = this.reportQuery.filters.getValue();
    let {sort, order} = event.detail;
    if (order === 'none') {
      sort = undefined;
      order = undefined;
    }
    this.reportData.nodesListParams.sort = sort;
    this.reportData.nodesListParams.order = order;
    this.getData(filters);
  }

  displayControlTitleOrId(title, id) {
    if (title === '') {
      return id;
    }
    return title;
  }

  orderFor(sortKey) {
    const {sort, order} = this.reportData.nodesListParams;
    return sortKey === sort ? order : 'none';
  }

  filterFor(node) {
    return this.reportQuery.filters.getValue().filter(f => {
      return f.type && f.type.name === 'node_id' && f.value.id === node.id;
    })[0];
  }

  hasFilter(node) {
    return this.filterFor(node) !== undefined;
  }

  addFilter(node) {
    const filter = {type: {name: 'node_id'}, value: {...node, text: node.name}};
    this.reportQuery.addFilter(filter);
  }

  removeFilter(node) {
    const filter = this.filterFor(node);
    this.reportQuery.removeFilter(filter);
  }

  getData(filters) {
    if (filters.length === 0) { return; }
    const params = this.reportData.nodesListParams;
    this.reportData.getReportingNodesList(filters, params);
  }

  // scan results component data
  displayScanResults(node) {
    const filters = this.reportQuery.filters.getValue();
    this.getProfilesForNode(filters, paginationOverride, node);
    this.displayScanResultsSidebar = true;
    this.scanResultsPane = 0;
  }

  hideScanResults() {
    this.displayScanResultsSidebar = false;
  }

  getProfilesForNode(filters: any[], listParams: any, node) {
    const nodeFilter = {type: {name: 'node_id'}, value: node};

    filters = [nodeFilter].concat(filters);
    this.scanResultsNodeFilter = nodeFilter;

    this.statsService.getStatsProfiles(filters, listParams).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(data => {
        const profiles = this.addProfileStatus(data);
        this.layerOneData = Object.assign({'id': node.name}, groupBy(profiles, 'status'));
      });
  }

  addProfileStatus(data) {
    return data.items.map(profile => {
      if (profile.failures === 0) {
        if (profile.passed > 0) {
          profile.status = 'passed';
        } else {
          profile.status = 'skipped';
        }
      } else {
        profile.status = 'failed';
      }
      return profile;
    });
  }

  getControls(item) {
    let filters = this.reportQuery.filters.getValue();

    this.scanResultsProfileFilter = {
      type: { name: 'profile_id' },
      value: { text: item.id}
    };

    filters = [this.scanResultsNodeFilter, this.scanResultsProfileFilter].concat(filters);

    this.statsService.getReports(filters, {sort: 'latest_report.end_time', order: 'DESC'}).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(data => {
        const reportID = data[0].id;
        // get single report with all filters
        this.getFilteredSingleReport(reportID, filters, item);
      });
  }

  getFilteredSingleReport(reportID, filters, item) {
    this.statsService.getSingleReport(reportID, filters).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(data => {
        const controls = this.addControlStatus(data);
        this.layerTwoData = Object.assign({'id': item.name }, groupBy(controls, 'status'));
        this.scanResultsPane = 1;
      });
  }

  addControlStatus(data) {
    return data.profiles[0].controls.map(control => {
      if (control.results) {
        const isSkipped = control.results.every(r => r.status === 'skipped');
        const isFailed = control.results.some(r => r.status === 'failed');
        if (isSkipped) {
          control.status = 'skipped';
        } else if (isFailed) {
          control.status = 'failed';
        } else {
          control.status = 'passed';
        }
      }
      return control;
    });
  }

  setControl(item) {
    this.control = item;
  }

  statusIcon(status) {
    switch (status) {
      case ('failed'): return 'report_problem';
      case ('passed'): return 'check_circle';
      case ('skipped'): return 'help';
      default: return '';
    }
  }

  statusControls(controls) {
    const failed = controls.failed.total;
    const skipped = controls.skipped.total;
    const passed = controls.passed.total;
    switch (true) {
      case (failed > 0): return 'failed';
      case (passed > 0 || skipped === 0): return 'passed';
      case (passed === 0 && skipped > 0): return 'skipped';
      default: return '';
    }
  }

  statusControlsSeverity(controls) {
    const status = this.statusControls(controls);
    switch (status) {
      case ('failed'): return `${controls.failed.total} Failed`;
      case ('passed'): return 'Passed';
      case ('skipped'): return 'Skipped';
      default: return '';
    }
  }

  formatTime(datestamp) {
    return datestamp ? moment(datestamp).fromNow() : '-';
  }

  impactStatus({ impact }) {
    if (impact >= 0.7) { return 'critical'; }
    if (impact >= 0.4) { return 'major'; }
    return 'minor';
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
}
