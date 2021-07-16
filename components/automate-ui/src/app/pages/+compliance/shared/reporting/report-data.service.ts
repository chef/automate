import { Injectable } from '@angular/core';
import { remove } from 'lodash';
import { StatsService } from './stats.service';
import { TelemetryService } from '../../../../services/telemetry/telemetry.service';
import { ReportQuery } from './report-query.service';

export type ReportingSummaryStatus = 'failed' | 'passed' | 'waived' | 'skipped' | 'unknown';

export interface ReportingSummary {
  stats: {
    nodes: any;
    profiles: any;
    controls?: any;
    platforms: number;
    environments: number;
  };
  status: ReportingSummaryStatus;
}

@Injectable()
export class ReportDataService {
  reportingSummaryEmpty = true;
  reportingSummary: ReportingSummary;

  nodesListLoading = true;
  nodesListEmpty = false;
  nodesList: any = {
    items: [],
    total: 0,
    total_failed: 0,
    total_passed: 0,
    total_skipped: 0,
    total_waived: 0
  };
  nodesListParams: any = {
    perPage: 100,
    page: 1,
    sort: 'latest_report.end_time',
    order: 'desc'
  };
  filteredNodesCount: number;

  profilesListLoading = true;
  profilesListParams: any = {
    perPage: 100,
    page: 1
  };
  profilesListEmpty = false;
  profilesList: any = {
    items: [],
    total: 0,
    failed: 0,
    passed: 0,
    skipped: 0,
    waived: 0
  };
  filteredProfileCount: number;

  controlsListLoading = true;
  controlsListParams: any = {
    perPage: 100,
    page: 1
  };
  controlsListEmpty = false;
  controlsList: any = {
    items: [],
    total: 0
  };

  constructor(
    private statsService: StatsService,
    private telemetryService: TelemetryService
  ) {}

  getReportingSummary(reportQuery: ReportQuery) {
    this.statsService.getSummary(reportQuery)
      .subscribe(data => {
        this.reportingSummaryEmpty = this.isAllZeros(data.stats);
        this.reportingSummary = data;
        // Data will give us nodes, platforms, profiles and environments
        // We will also report filters so that we know when these counts are constrained.
        if (data !== null) {
          const summaryStats = data.stats;
          this.telemetryService.track('complianceCountsWithFilters',
            {summaryStats, filters: reportQuery.filters});
        }
      });
  }

  getReportingNodesList(reportQuery: ReportQuery, listParams: any, nodeFilterStatus: string) {
    this.nodesListLoading = true;
    this.statsService.getNodes(reportQuery, listParams)
      .subscribe(data => {
        this.nodesListLoading = false;
        this.nodesListEmpty = this.isZero(data.total);
        let status: string;
        if (nodeFilterStatus === 'all') {
          status = 'total';
        } else {
          status = 'total_' + nodeFilterStatus;
        }
        this.nodesList = Object.assign({}, this.nodesList, data);
        if (status in this.nodesList) {
          this.filteredNodesCount = this.nodesList[status];
        }
      });
  }

  getReportingProfilesList(reportQuery: ReportQuery, listParams: any, profileFilterStatus: string) {
    this.profilesListLoading = true;
    this.statsService.getProfiles(reportQuery, listParams)
      .subscribe(data => {
        this.profilesListLoading = false;
        this.profilesListEmpty = this.isEmpty(data.items);
        let status: string;
        if (profileFilterStatus === 'all') {
          status = 'total';
        } else {
          status = profileFilterStatus;
        }
        this.profilesList = Object.assign({}, this.profilesList, data);
        if (status in this.profilesList) {
          this.filteredProfileCount = this.profilesList[status];
        }
      });
  }

  getReportingControlsList(reportQuery: ReportQuery) {
    this.controlsListLoading = true;
    this.statsService.getControls(reportQuery)
      .subscribe(data => {
        this.controlsListLoading = false;
        this.controlsListEmpty = this.isEmpty(data.items);
        this.controlsList = Object.assign({}, this.controlsList, data);
      });
  }

  isEmpty(data) {
    return data === undefined || data.length === 0 || (data.length === 1 && data[0] === 'passed');
  }

  isZero(count: number): boolean {
    return count === 0;
  }

  isAllZeros(data) {
    const array = Object.keys(data).map(itm => data[itm]);
    const result = remove(array, function (value) {
      return value !== 0;
    });
    return this.isEmpty(result);
  }
}
