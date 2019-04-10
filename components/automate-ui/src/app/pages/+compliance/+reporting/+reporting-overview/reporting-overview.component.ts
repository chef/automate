import { takeUntil } from 'rxjs/operators';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subject } from 'rxjs';
import {
  StatsService,
  ReportQueryService,
  ReportDataService
} from '../../shared/reporting';

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

  // Used to notify all subscriptions to unsubscribe
  // http://stackoverflow.com/a/41177163/319074
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

  onSelectedTab(event) {
    this.selectedButtonTab = event.target.value;
    this.getData(this.reportQuery.filters.getValue());
  }

  onTrendIntervalChange(event) {
    const interval = event.target.value;
    const {intervals, endDate} = this.reportQuery;
    const startDate = intervals[interval][1](endDate);
    this.reportQuery.interval = interval;
    this.reportQuery.setDateRange(startDate, endDate);
  }

  getData(filters) {
    if (filters.length === 0) { return; }
    if (this.selectedButtonTab === 'Node Status') {
      this.getNodeStatusData(filters);
    } else {
      this.getProfileStatusData(filters);
    }
  }

  getNodeStatusData(filters) {
    this.getNodeStatusFailures(filters);
    this.getNodeSummary(filters);
    this.getNodeTrend(filters);
  }

  getProfileStatusData(filters) {
    this.getProfileStatusFailures(filters);
    this.getControlsSummary(filters);
    this.getControlsTrend(filters);
  }

  getNodeStatusFailures(filters) {
    this.nodeBubbleLoading = true;
    this.statsService.getFailures(['platform', 'environment'], filters).pipe(
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

  getProfileStatusFailures(filters) {
    this.profileBubbleLoading = true;
    this.statsService.getFailures(['profile', 'control'], filters).pipe(
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

  getNodeTrend(filters) {
    this.nodeTrendLoading = true;
    this.statsService.getNodeTrend(filters).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(data => {
        this.nodeTrendLoading = false;
        this.nodeTrendData = data;
    });
  }

  getControlsTrend(filters) {
    this.profileTrendLoading = true;
    this.statsService.getControlsTrend(filters).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(data => {
        this.profileTrendLoading = false;
        this.profileTrendData = data;
      });
  }

  getNodeSummary(filters) {
    this.nodeRadialLoading = true;
    this.statsService.getNodeSummary(filters).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(data => {
        this.nodeRadialLoading = false;
        this.nodeRadialEmpty = this.reportData.isAllZeros(data);
        this.nodeRadialData = this.formatNodeRadialData(data);
      });
  }

  getControlsSummary(filters) {
    this.profileRadialLoading = true;
    this.statsService.getControlsSummary(filters).pipe(
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
      critical: data.high_risk,
      major: data.medium_risk,
      minor: data.low_risk
    };
  }

  formatProfileRadialData(data) {
    return {
      passed: data.passed,
      skipped: data.skipped,
      failed: data.failures,
      critical: data.criticals,
      major: data.majors,
      minor: data.minors
    };
  }
}
