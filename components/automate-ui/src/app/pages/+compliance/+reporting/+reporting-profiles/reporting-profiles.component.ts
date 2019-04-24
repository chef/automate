import { takeUntil } from 'rxjs/operators';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subject } from 'rxjs';
import { groupBy } from 'lodash';
import { Filter } from '../types';
import { paginationOverride } from '../shared';
import { StatsService, ReportQueryService, ReportDataService } from '../../shared/reporting';
import { ChefSessionService } from '../../../../services/chef-session/chef-session.service';
import * as moment from 'moment';

@Component({
  selector: 'app-reporting-profiles',
  templateUrl: './reporting-profiles.component.html',
  styleUrls: [ './reporting-profiles.component.scss' ]
})
export class ReportingProfilesComponent implements OnInit, OnDestroy {
  user: String;
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
    private chefSessionService: ChefSessionService,
    public reportQuery: ReportQueryService,
    public reportData: ReportDataService
  ) {}

  ngOnInit() {
    this.user = this.chefSessionService.username;
    this.reportQuery.filters.pipe(
      takeUntil(this.isDestroyed))
      .subscribe(this.getData.bind(this));
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  onProfilesListPageChanged(event) {
    const filters = this.reportQuery.filters.getValue();
    this.reportData.profilesListParams.page = event;
    this.getData(filters);
  }

  onProfilesListSortToggled(event) {
    const filters = this.reportQuery.filters.getValue();
    let {sort, order} = event.detail;
    if (order === 'none') {
      sort = undefined;
      order = undefined;
    }
    this.reportData.profilesListParams.sort = sort;
    this.reportData.profilesListParams.order = order;
    this.getData(filters);
  }

  orderFor(sortKey) {
    const {sort, order} = this.reportData.profilesListParams;
    return sortKey === sort ? order : 'none';
  }

  filterFor(profile) {
    return this.reportQuery.filters.getValue().filter(f => {
      return f.type && f.type.name === 'profile' && f.value.id === profile.id;
    })[0];
  }

  hasFilter(profile) {
    return this.filterFor(profile) !== undefined;
  }

  addFilter(profile) {
    const filter = {type: {name: 'profile'}, value: {...profile, text: profile.name}};
    this.reportQuery.addFilter(filter);
  }

  removeFilter(profile) {
    const filter = this.filterFor(profile);
    this.reportQuery.removeFilter(filter);
  }

  getData(filters) {
    if (filters.length === 0) { return; }
    const params = this.reportData.profilesListParams;
    this.reportData.getReportingProfilesList(filters, params);
  }

  displayScanResults(profile) {
    const filters = this.reportQuery.filters.getValue();
    this.getNodesForProfile(filters, paginationOverride, profile);
    this.displayScanResultsSidebar = true;
    this.scanResultsPane = 0;
  }

  hideScanResults() {
    this.displayScanResultsSidebar = false;
  }

  getNodesForProfile(filters: any[], listParams: any, profile) {
    const profileFilter = {type: {name: 'profile_id'}, value: profile};
    filters = [profileFilter].concat(filters);

    this.scanResultsProfileFilter = {
      type: { name: 'profile_id' },
      value: { text: profile.id }
    };

    this.statsService.getNodes(filters, listParams).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(data => {
        const nodes = data.items.map(node => {
          node.status = node.latest_report.status;
          return node;
        });
        this.layerOneData = Object.assign({'id': profile.title}, groupBy(nodes, 'status'));
      });
  }

  getControls(item) {
    let filters = this.reportQuery.filters.getValue();
    this.scanResultsNodeFilter = {
      type: {name: 'node_id'},
      value: {id: item.id}
    };
    filters = [this.scanResultsNodeFilter, this.scanResultsProfileFilter].concat(filters);

    this.statsService.getReports(filters, {sort: 'latest_report.end_time', order: 'DESC'}).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(data => {
        // get report ID and set profile filter value if not already set
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

  formatDaysAgo(timestamp) {
    return moment(timestamp).fromNow();
  }
}
