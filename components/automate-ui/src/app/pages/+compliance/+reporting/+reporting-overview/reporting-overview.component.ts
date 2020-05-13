import { takeUntil, map } from 'rxjs/operators';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subject, Observable } from 'rxjs';
import {
  StatsService,
  ReportQueryService,
  ReportDataService,
  ReportQuery
} from '../../shared/reporting';
import { ActivatedRoute, Router } from '@angular/router';
import { union } from 'lodash/fp';
import * as moment from 'moment/moment';

type Tab = 'Node Status' | 'Profile Status';

@Component({
  selector: 'app-reporting-overview',
  templateUrl: './reporting-overview.component.html',
  styleUrls: [ './reporting-overview.component.scss' ],
  providers: [StatsService]
})
export class ReportingOverviewComponent implements OnInit, OnDestroy {
  buttonTabItems: Array<Tab> = ['Node Status', 'Profile Status'];
  selectedButtonTab: Tab = 'Node Status';

  // Node tab graphs
  nodeRadialTitle = 'Global Compliance';
  nodeRadialBarsTitle = 'Severity of Node Failures';
  nodeRadialData = {};
  nodeBubbleLeftTitle = 'Top Platform Failures';
  bubblePlatformFailures = [];
  nodeBubbleRightTitle = 'Top Environment Failures';
  bubbleEnvFailures = [];
  nodeTrendTitle = 'Global Compliance Over Time';
  nodeTrendData: Array<{}> = [];
  progressBarsVisible = true;

  // Loading components
  nodeRadialLoading = false;
  nodeTrendLoading = false;
  nodeBubbleLoading = false;

  envBubbleEmpty = false;
  platformBubbleEmpty = false;
  nodeRadialEmpty = false;
  profileRadialEmpty = false;
  profileBubbleEmpty = false;
  controlBubbleEmpty = false;

  // Profile tab graphs
  profileRadialTitle = 'Control Status';
  profileRadialBarsTitle = 'Severity of Control Failures';
  profileRadialData = {};
  profileBubbleLeftTitle = 'Top Profile Failures';
  bubbleProfileFailures = [];
  profileBubbleRightTitle = 'Top Control Failures';
  bubbleControlFailures = [];
  profileTrendTitle = 'Test Results Over Time';
  profileTrendData: Array<{}> = [];

  profileRadialLoading = false;
  profileTrendLoading = false;
  profileBubbleLoading = false;

  tooltipText = 'Failed Nodes';
  interval$: Observable<number>;

  // Used to notify all subscriptions to unsubscribe
  // http://stackoverflow.com/a/41177163/319074
  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    private statsService: StatsService,
    public reportQuery: ReportQueryService,
    public reportData: ReportDataService,
    private route: ActivatedRoute,
    private router: Router
  ) {}

  ngOnInit() {
    this.reportQuery.state.pipe(
      takeUntil(this.isDestroyed))
      .subscribe(this.getData.bind(this));

    this.interval$ = this.reportQuery.state.pipe(map((reportQuery: ReportQuery) =>
      reportQuery.interval));
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  onSelectedTab(event) {
    this.selectedButtonTab = event.target.value;
    this.getData(this.reportQuery.getReportQuery());
  }

  onTrendIntervalChange(event) {
    const interval = event.target.value;
    const queryParams = {...this.route.snapshot.queryParams};
    if (interval === '0' ) {
      delete queryParams['date_interval'];
    } else {
      queryParams['date_interval'] = interval;
    }

    this.router.navigate([], {queryParams});
  }

  getData(reportQuery: ReportQuery) {
    if (this.selectedButtonTab === 'Node Status') {
      this.getNodeStatusData(reportQuery);
    } else {
      this.getProfileStatusData(reportQuery);
    }
  }

  onPlatformChanged(platformItem) {
    if (platformItem && platformItem.name) {
      this.updateUrlWithFilter(platformItem.name + '*', 'platform_with_version');
    }
  }

  onEnvironmentChanged(environmentItem) {
    if (environmentItem && environmentItem.name) {
      this.updateUrlWithFilter(environmentItem.name, 'environment');
    }
  }

  onProfileChanged(profileItem) {
    const typeName = 'profile_id';
    if (profileItem && profileItem.id) {
      this.reportQuery.setFilterTitle(typeName, profileItem.id, profileItem.name);
      this.updateUrlWithFilter(profileItem.id, typeName);
    }
  }

  onControlChanged(controlItem) {
    if (controlItem && controlItem.name) {
      this.updateUrlWithFilter(controlItem.name, 'control_id');
    }
  }

  onDateChanged(endDate) {
    const queryParams = {...this.route.snapshot.queryParams};
    const endDateMoment = moment.utc(endDate);
    if (endDate && endDateMoment.isValid()) {
      if (moment.utc().format('YYYY-MM-DD') === endDateMoment.format('YYYY-MM-DD')) {
        delete queryParams['end_time'];
      } else {
        queryParams['end_time'] = endDateMoment.format('YYYY-MM-DD');
      }

      this.router.navigate([], {queryParams});
    }
  }

  getNodeStatusData(reportQuery: ReportQuery) {
    this.getNodeStatusFailures(reportQuery);
    this.getNodeSummary(reportQuery);
    this.getNodeTrend(reportQuery);
  }

  getProfileStatusData(reportQuery: ReportQuery) {
    this.getProfileStatusFailures(reportQuery);
    this.getControlsSummary(reportQuery);
    this.getControlsTrend(reportQuery);
  }

  getNodeStatusFailures(reportQuery: ReportQuery) {
    this.nodeBubbleLoading = true;
    this.statsService.getFailures(['platform', 'environment'], reportQuery).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(data => {
        this.bubblePlatformFailures = data['platforms'];
        this.bubbleEnvFailures = data['environments'];
        // turn off loading components
        this.nodeBubbleLoading = false;
        // check if empty
        this.platformBubbleEmpty = this.reportData.isEmpty(data['platforms']);
        this.envBubbleEmpty = this.reportData.isEmpty(data['environments']);
      });
  }

  getProfileStatusFailures(reportQuery: ReportQuery) {
    this.profileBubbleLoading = true;
    this.statsService.getFailures(['profile', 'control'], reportQuery).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(data => {
        this.bubbleProfileFailures = data['profiles'];
        this.bubbleControlFailures = data['controls'];
        // turn off loading components
        this.profileBubbleLoading = false;
        // check if empty
        this.profileBubbleEmpty = this.reportData.isEmpty(data['profiles']);
        this.controlBubbleEmpty = this.reportData.isEmpty(data['controls']);
      });
  }

  getNodeTrend(reportQuery: ReportQuery) {
    this.nodeTrendLoading = true;
    this.statsService.getNodeTrend(reportQuery).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(data => {
        this.nodeTrendLoading = false;
        this.nodeTrendData = data;
    });
  }

  getControlsTrend(reportQuery: ReportQuery) {
    this.profileTrendLoading = true;
    this.statsService.getControlsTrend(reportQuery).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(data => {
        this.profileTrendLoading = false;
        this.profileTrendData = data;
      });
  }

  getNodeSummary(reportQuery: ReportQuery) {
    this.nodeRadialLoading = true;
    this.statsService.getNodeSummary(reportQuery).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(data => {
        this.nodeRadialLoading = false;
        this.nodeRadialEmpty = this.reportData.isAllZeros(data);
        this.nodeRadialData = this.formatNodeRadialData(data);
      });
  }

  getControlsSummary(reportQuery: ReportQuery) {
    this.profileRadialLoading = true;
    this.statsService.getControlsSummary(reportQuery).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(data => {
        this.profileRadialLoading = false;
        this.profileRadialData = this.formatProfileRadialData(data);
        this.profileRadialEmpty = this.reportData.isAllZeros(data);
      });
  }

  formatNodeRadialData(data) {
    return {
      passed: data.compliant,
      skipped: data.skipped,
      failed: data.noncompliant,
      waived: data.waived,
      critical: data.high_risk,
      major: data.medium_risk,
      minor: data.low_risk
    };
  }

  formatProfileRadialData(data) {
    return {
      passed: data.passed,
      skipped: data.skipped,
      waived: data.waived,
      failed: data.failures,
      critical: data.criticals,
      major: data.majors,
      minor: data.minors
    };
  }

  private updateUrlWithFilter(value: string, typeName: string) {
    const {queryParamMap} = this.route.snapshot;
    const queryParams = {...this.route.snapshot.queryParams};
    const existingValues = union([value], queryParamMap.getAll(typeName));

    queryParams[typeName] = existingValues;

    this.router.navigate(['/compliance', 'reports', 'nodes'], {queryParams});
  }
}
