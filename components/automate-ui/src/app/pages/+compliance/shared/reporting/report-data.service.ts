import { Injectable } from '@angular/core';
import { remove } from 'lodash';
import { StatsService } from './stats.service';
import { TelemetryService } from '../../../../services/telemetry/telemetry.service';

interface ReportingSummary {
  stats: {
    nodes: any;
    profiles: any;
  };
}

@Injectable()
export class ReportDataService {
  reportingSummaryEmpty = true;
  reportingSummary: ReportingSummary;

  nodesListLoading = true;
  nodesListEmpty = false;
  nodesList: any = {
    items: [],
    total: 0
  };
  nodesListParams: any = {
    perPage: 100,
    page: 1,
    sort: 'latest_report.end_time',
    order: 'desc'
  };

  profilesListLoading = true;
  profilesListParams: any = {
    perPage: 100,
    page: 1
  };
  profilesListEmpty = false;
  profilesList: any = {
    items: [],
    total: 0
  };

  constructor(
    private statsService: StatsService,
    private telemetryService: TelemetryService
  ) {}

  getReportingSummary(filters) {
    this.statsService.getSummary(filters)
      .subscribe(data => {
        this.reportingSummaryEmpty = this.isAllZeros(data.stats);
        this.reportingSummary = data;
        // Data will give us nodes, platforms, profiles and environments
        // We will also report filters so that we know when these counts are constrained.
        if (data !== null) {
          const summaryStats = data.stats;
          this.telemetryService.track('complianceCountsWithFilters', {summaryStats, filters});
        }
      });
  }

  getReportingNodesList(filters: any[], listParams: any) {
    this.nodesListLoading = true;
    this.statsService.getNodes(filters, listParams)
      .subscribe(data => {
        this.nodesListLoading = false;
        this.nodesListEmpty = this.isEmpty(data.items);
        this.nodesList = Object.assign({}, this.nodesList, data);
      });
  }

  getReportingProfilesList(filters: any[], listParams: any) {
    this.profilesListLoading = true;
    this.statsService.getProfiles(filters, listParams)
      .subscribe(data => {
        this.profilesListLoading = false;
        this.profilesListEmpty = this.isEmpty(data.items);
        this.profilesList = Object.assign({}, this.profilesList, data);
      });
  }

  isEmpty(data) {
    if (data === undefined || data.length === 0 || (data.length === 1 && data[0] === 'passed')) {
      return true;
    }
    return false;
  }

  isAllZeros(data) {
    const array = Object.keys(data).map(itm => data[itm]);
    const result = remove(array, function (value) {
      return value !== 0;
    });
    return this.isEmpty(result);
  }
}
