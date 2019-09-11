import { takeUntil } from 'rxjs/operators';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subject } from 'rxjs';
import { groupBy } from 'lodash';
import * as moment from 'moment';
import { FilterC } from '../types';
import { paginationOverride } from '../shared';
import {
  StatsService,
  ReportQueryService,
  ReportDataService,
  ReportQuery } from '../../shared/reporting';
import { ActivatedRoute, Router } from '@angular/router';

@Component({
  selector: 'app-reporting-nodes',
  templateUrl: './reporting-nodes.component.html',
  styleUrls: [ './reporting-nodes.component.scss' ]
})
export class ReportingNodesComponent implements OnInit, OnDestroy {
  displayScanResultsSidebar = false;
  scanResultsNodeFilter: FilterC;
  scanResultsProfileFilter: FilterC;
  layerOneData: any = {};
  layerTwoData: any = {};
  control: any = {};
  nodeFilterStatus = 'all';

  scanResultsPane = 0;
  openControls = {};

  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    private statsService: StatsService,
    public reportQuery: ReportQueryService,
    public reportData: ReportDataService,
    private router: Router,
    private route: ActivatedRoute
  ) {}

  ngOnInit() {
    this.reportQuery.state.pipe(
      takeUntil(this.isDestroyed))
      .subscribe(this.getData.bind(this));
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  filterNodeStatus(_event, status) {
    this.nodeFilterStatus = status;
  }

  isNodeStatusSelected(status): boolean {
    return this.nodeFilterStatus === status;
  }

  filteredNodes(nodes) {
    return this.nodeFilterStatus === 'all'
      ? nodes
      : nodes.filter((node) => node.latest_report.status === this.nodeFilterStatus);
  }

  onNodesListPageChanged(event) {
    const reportQuery = this.reportQuery.getReportQuery();
    this.reportData.nodesListParams.page = event;
    this.getData(reportQuery);
  }

  onNodesListSortToggled(event) {
    const reportQuery = this.reportQuery.getReportQuery();
    let {sort, order} = event.detail;
    if (order === 'none') {
      sort = undefined;
      order = undefined;
    }
    this.reportData.nodesListParams.sort = sort;
    this.reportData.nodesListParams.order = order;
    this.getData(reportQuery);
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

  filterFor(node): FilterC {
    return this.reportQuery.getReportQuery().filters.filter(f => {
      return f.type && f.type.name === 'node_id' && f.value.id === node.id;
    })[0];
  }

  hasFilter(node) {
    return this.filterFor(node) !== undefined;
  }

  addFilter(node) {
    const typeName = 'node_id';

    this.reportQuery.setFilterTitle(typeName, node.id, node.name);

    const {queryParamMap} = this.route.snapshot;
    const queryParams = {...this.route.snapshot.queryParams};
    const existingValues = queryParamMap.getAll(typeName).filter(
      v => v !== node.id).concat(node.id);

    queryParams[typeName] = existingValues;

    this.router.navigate([], {queryParams});
  }

  removeFilter(node) {
    const filter = this.filterFor(node);
    const typeName = filter.type.name;

    const {queryParamMap} = this.route.snapshot;
    const queryParams = {...this.route.snapshot.queryParams};
    const values = queryParamMap.getAll(typeName).filter(v => v !== filter.value.id);

    if (values.length === 0) {
      delete queryParams[typeName];
    } else {
      queryParams[typeName] = values;
    }

    this.router.navigate([], {queryParams});
  }

  getData(filters) {
    if (filters.length === 0) { return; }
    const params = this.reportData.nodesListParams;
    this.reportData.getReportingNodesList(filters, params);
  }

  // scan results component data
  displayScanResults(node) {
    const reportQuery = this.reportQuery.getReportQuery();
    this.getProfilesForNode(reportQuery, paginationOverride, node);
    this.displayScanResultsSidebar = true;
    this.scanResultsPane = 0;
  }

  hideScanResults() {
    this.displayScanResultsSidebar = false;
  }

  getProfilesForNode(reportQuery: ReportQuery, listParams: any, node) {
    const nodeFilter: FilterC = {type: {name: 'node_id'}, value: { text: node.name, id: node.id}};

    reportQuery.filters = [nodeFilter].concat(reportQuery.filters);
    this.scanResultsNodeFilter = nodeFilter;

    this.statsService.getStatsProfiles(reportQuery, listParams).pipe(
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
    const reportQuery = this.reportQuery.getReportQuery();

    this.scanResultsProfileFilter = {
      type: { name: 'profile_id' },
      value: { text: item.id}
    };

    reportQuery.filters = [this.scanResultsNodeFilter, this.scanResultsProfileFilter].concat(
      reportQuery.filters);

    this.statsService.getReports(reportQuery, {sort: 'latest_report.end_time', order: 'DESC'}).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(data => {
        const reportID = data[0].id;
        // get single report with all filters
        this.getFilteredSingleReport(reportID, reportQuery, item);
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

  statusControlsSeverity(controls, status) {
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
