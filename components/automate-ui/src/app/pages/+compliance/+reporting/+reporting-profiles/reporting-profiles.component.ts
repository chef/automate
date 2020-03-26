import { takeUntil } from 'rxjs/operators';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subject } from 'rxjs';
import { groupBy } from 'lodash';
import { FilterC } from '../types';
import { paginationOverride } from '../shared';
import { StatsService, ReportQueryService, ReportDataService,
  ReportQuery } from '../../shared/reporting';
import { ChefSessionService } from '../../../../services/chef-session/chef-session.service';
import * as moment from 'moment';
import { ActivatedRoute, Router } from '@angular/router';

@Component({
  selector: 'app-reporting-profiles',
  templateUrl: './reporting-profiles.component.html',
  styleUrls: [ './reporting-profiles.component.scss' ]
})
export class ReportingProfilesComponent implements OnInit, OnDestroy {
  user: String;
  displayScanResultsSidebar = false;
  scanResultsNodeFilter: FilterC;
  scanResultsProfileFilter: FilterC;
  layerOneData: any = {};
  layerTwoData: any = {};
  control: any = {};
  profileFilterStatus = 'all';

  scanResultsPane = 0;
  openControls = {};

  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    private statsService: StatsService,
    private chefSessionService: ChefSessionService,
    public reportQuery: ReportQueryService,
    public reportData: ReportDataService,
    private router: Router,
    private route: ActivatedRoute
  ) {}

  ngOnInit() {
    this.user = this.chefSessionService.username;
    this.reportQuery.state.pipe(
      takeUntil(this.isDestroyed))
      .subscribe(this.getData.bind(this));
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  filterProfileStatus(_event, status) {
    this.profileFilterStatus = status;
  }

  isProfileStatusSelected(status): boolean {
    return this.profileFilterStatus === status;
  }

  filteredProfiles(profiles) {
    return this.profileFilterStatus === 'all'
      ? profiles
      : profiles.filter((profile) => profile.status === this.profileFilterStatus);
  }

  onProfilesListPageChanged(event) {
    const reportQuery = this.reportQuery.getReportQuery();
    this.reportData.profilesListParams.page = event;
    this.getData(reportQuery);
  }

  onProfilesListSortToggled(event) {
    const reportQuery = this.reportQuery.getReportQuery();
    let {sort, order} = event.detail;
    if (order === 'none') {
      sort = undefined;
      order = undefined;
    }
    this.reportData.profilesListParams.sort = sort;
    this.reportData.profilesListParams.order = order;
    this.getData(reportQuery);
  }

  orderFor(sortKey) {
    const {sort, order} = this.reportData.profilesListParams;
    return sortKey === sort ? order : 'none';
  }

  filterFor(profile) {
    return this.reportQuery.getReportQuery().filters.filter(f => {
      return f.type && f.type.name === 'profile_id' && f.value.id === profile.id;
    })[0];
  }

  hasFilter(profile) {
    return this.filterFor(profile) !== undefined;
  }

  addFilter(profile) {
    const typeName = 'profile_id';
    const title = `${profile.title}, v${profile.version}`;

    this.reportQuery.setFilterTitle(typeName, profile.id, title);

    const {queryParamMap} = this.route.snapshot;
    const queryParams = {...this.route.snapshot.queryParams};
    const existingValues = queryParamMap.getAll(typeName).filter(
      v => v !== profile.id).concat(profile.id);

    queryParams[typeName] = existingValues;

    this.router.navigate([], {queryParams});
  }

  removeFilter(profile) {
    const filter = this.filterFor(profile);
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

  getData(reportQuery: ReportQuery) {
    const params = this.reportData.profilesListParams;
    this.reportData.getReportingProfilesList(reportQuery, params);
  }

  displayScanResults(profile) {
    const reportQuery = this.reportQuery.getReportQuery();
    this.getNodesForProfile(reportQuery, paginationOverride, profile);
    this.displayScanResultsSidebar = true;
    this.scanResultsPane = 0;
  }

  hideScanResults() {
    this.displayScanResultsSidebar = false;
  }

  getNodesForProfile(reportQuery: ReportQuery, listParams: any, profile) {
    const profileFilter: FilterC = {type: {name: 'profile_id'},
      value: {text: profile.name, id: profile.id}};
    reportQuery.filters = [profileFilter].concat(reportQuery.filters);

    this.scanResultsProfileFilter = {
      type: { name: 'profile_id' },
      value: { text: profile.id }
    };
    listParams['sort'] = 'latest_report.end_time';
    listParams['order'] = 'desc';
    this.statsService.getNodes(reportQuery, listParams).pipe(
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
    const reportQuery: ReportQuery = this.reportQuery.getReportQuery();
    this.scanResultsNodeFilter = {
      type: {name: 'node_id'},
      value: {id: item.id}
    };
    reportQuery.filters = [this.scanResultsNodeFilter,
      this.scanResultsProfileFilter].concat(reportQuery.filters);

    this.statsService.getReports(reportQuery, {sort: 'latest_report.end_time', order: 'DESC'}).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(data => {
        // get report ID and set profile filter value if not already set
        const reportID = data[0].id;
        // get single report with all filters
        this.getFilteredSingleReport(reportID, reportQuery, item);
      });
  }

  getFilteredSingleReport(reportID, filters, item) {
    this.statsService.getSingleReport(reportID, filters).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(data => {
        const controls = data.profiles[0].controls;
        this.layerTwoData = Object.assign({'id': item.name }, groupBy(controls, 'status'));
        this.scanResultsPane = 1;
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
