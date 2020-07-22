import { map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient, HttpParams } from '@angular/common/http';
import { reduce } from 'lodash/fp';
import { DailyCheckInCountCollection, DailyCheckInCount,
  NodeRunsDailyStatusCollection,
  TopErrorsCollection, TopErrorsItem,
  CountedDurationCollection, CountedDurationItem, Desktop, Filter,
  NodeMetadataCount, NodeMetadataCountType, TermFilter } from './desktop.model';

import { environment } from 'environments/environment';
const CONFIG_MGMT_URL = environment.config_mgmt_url;

interface RespDailyCheckInCountCollection {
  counts: RespDailyCheckInCount[];
}

interface RespDailyCheckInCount {
  start: Date;
  end: Date;
  count: number;
  total: number;
}

interface RespNodeRunsDailyStatusCollection {
  durations: RespNodeRunsDailyStatus[];
}

interface RespNodeRunsDailyStatus {
  start: Date;
  end: Date;
  status: string;
  run_id: string;
}

interface RespTopNodeErrors {
  errors: RespErrorItem[];
}

interface RespErrorItem {
  count: number;
  type: string;
  error_message: string;
}

interface RespCountedDurationCollection {
  counted_durations: RespCountedDurationItem[];
}

interface RespCountedDurationItem {
  duration: string;
  count: number;
}

interface RespNodeMetadataCounts {
  types: NodeMetadataCount[];
}

export interface RespDesktop {
  id: string;
  name: string;
  status: string;
  checkin: Date;
  uptime_seconds: number;
  platform: string;
  platform_family: string;
  platform_version: string;
  environment: string;
  policy_group: string;
  latest_run_id: string;
  fqdn: string;
  organization: string;
  source_fqdn: string;
  has_runs_data: boolean;
  last_ccr_received: Date;
  deprecations_count: number;
  chef_version: string;
  domain: string;
  tag: string;
  ipaddress: string;
  ip6address: string;
  macaddress: string;
  memory_total: string;
  virtualization_system: string;
  virtualization_role: string;
  kernel_release: string;
  kernel_version: string;
  hostname: string;
  timezone: string;
  dmi_system_manufacturer: string;
  dmi_system_serial_number: string;
  cloud_provider: string;
}

export interface RespDesktopCount {
  total: number;
}

@Injectable()
export class DesktopRequests {

  constructor(private http: HttpClient) { }

  public getDailyCheckInCountCollection(daysAgo: number): Observable<DailyCheckInCountCollection> {
    return this.http.get<RespDailyCheckInCountCollection>(
      `${CONFIG_MGMT_URL}/stats/checkin_counts_timeseries?days_ago=${daysAgo}`)
      .pipe(map(respDailyCheckInCountCollection =>
        this.createDailyCheckInCountCollection(respDailyCheckInCountCollection)));
  }

  public getDailyNodeRunsStatusCountCollection(nodeId: string, daysAgo: number):
    Observable<NodeRunsDailyStatusCollection> {
      return this.http.get<RespNodeRunsDailyStatusCollection>(
        `${CONFIG_MGMT_URL}/node_runs_daily_status_time_series?node_id=${nodeId}&days_ago=${daysAgo}`)
        .pipe(map(respNodeRunsDailyStatusCollection =>
          this.createNodeRunsDailyStatusCollection(respNodeRunsDailyStatusCollection)));
  }

  public getTopErrorsCollection(): Observable<TopErrorsCollection> {
    return this.http.get<RespTopNodeErrors>(`${CONFIG_MGMT_URL}/errors`)
    .pipe(map(respTopNodeErrors =>
      this.createTopErrorCollection(respTopNodeErrors)));
  }

  public getUnknownDesktopDurationCounts(): Observable<CountedDurationCollection> {
    const url = `${CONFIG_MGMT_URL}/stats/missing_node_duration_counts`;
    const options = {
      params: this.buildUnknownDesktopDurationCountsParams()
    };

    return this.http.get<RespCountedDurationCollection>(url, options)
    .pipe(map(respCountedDurationCollection =>
      this.createCountedDurationCollection(respCountedDurationCollection)));
  }

  public getNodeMetadataCounts(filter: Filter): Observable<NodeMetadataCount[]> {
    const url = `${CONFIG_MGMT_URL}/node_metadata_counts`;
    const options = { params: this.buildNodeMetadataCountsParams(filter) };

    return this.http.get<RespNodeMetadataCounts>(url, options).pipe(
      map(respNodeMetadataCounts => respNodeMetadataCounts.types));
  }

  public getDesktops(filter: Filter): Observable<Desktop[]> {
    const url = `${CONFIG_MGMT_URL}/nodes`;

    const options = {
      params: this.buildURLSearchParams(filter)
    };

    return this.http.get<RespDesktop[]>(url, options).pipe(
      map((res) => res.map((respDesktop: RespDesktop) => this.createDesktop(respDesktop))));
  }

  public getDesktopsTotal(filter: Filter): Observable<number> {
    const url = `${CONFIG_MGMT_URL}/stats/node_counts`;
    const options = {
      params: this.buildURLSearchParams(filter)
    };

    return this.http.get<RespDesktopCount>(url, options).pipe(map((res) => res.total));
  }

  private createDesktop(respDesktop: RespDesktop): Desktop {
    return {
      id: respDesktop.id,
      name: respDesktop.name,
      status: respDesktop.status,
      checkin: respDesktop.checkin,
      uptimeSeconds: respDesktop.uptime_seconds,
      platform: respDesktop.platform,
      platformFamily: respDesktop.platform_family,
      platformVersion: respDesktop.platform_version,
      chefVersion: respDesktop.chef_version,
      domain: respDesktop.domain,
      latestRunId: respDesktop.latest_run_id,
      environment: respDesktop.environment,
      tag: respDesktop.tag,
      ipaddress: respDesktop.ipaddress,
      ip6address: respDesktop.ip6address,
      macaddress: respDesktop.macaddress,
      memoryTotal: respDesktop.memory_total,
      virtualizationSystem: respDesktop.virtualization_system,
      virtualizationRole: respDesktop.virtualization_role,
      kernelRelease: respDesktop.kernel_release,
      kernelVersion: respDesktop.kernel_version,
      hostname: respDesktop.hostname,
      timezone: respDesktop.timezone,
      dmiSystemManufacturer: respDesktop.dmi_system_manufacturer,
      dmiSystemSerialNumber: respDesktop.dmi_system_serial_number,
      cloudProvider: respDesktop.cloud_provider
    };
  }

  private buildUnknownDesktopDurationCountsParams(): HttpParams {
    let searchParam = new HttpParams();

    searchParam = searchParam.append('durations', '1M');
    searchParam = searchParam.append('durations', '2w');
    searchParam = searchParam.append('durations', '1w');
    searchParam = searchParam.append('durations', '3d');

    return searchParam;
  }

  private buildNodeMetadataCountsParams(filter: Filter): HttpParams {
    let searchParam = new HttpParams();

    if (filter.terms.length > 0) {
      searchParam = this.formatFilterParams(searchParam, filter.terms);
    }

    if (filter.start) {
      searchParam = searchParam.append('start', this.formatDateParam(filter.start));
    }

    if (filter.end) {
      searchParam = searchParam.append('end', this.formatDateParam(filter.end));
    }

    searchParam = searchParam.append('type', NodeMetadataCountType.Platform);
    searchParam = searchParam.append('type', NodeMetadataCountType.Environment);
    searchParam = searchParam.append('type', NodeMetadataCountType.Domain);
    searchParam = searchParam.append('type', NodeMetadataCountType.Status);

    return searchParam;
  }

  private formatFilterParams(params: HttpParams, terms: TermFilter[]): HttpParams {
    return reduce((param, term) => {
      const filterParam = `${encodeURIComponent(term.type)}:${encodeURIComponent(term.value)}`;
      return param.append('filter', filterParam);
    }, params, terms);
  }

  private buildURLSearchParams(filter: Filter): HttpParams {
    let searchParam = new HttpParams();

    if (filter.terms.length > 0) {
      searchParam = this.formatFilterParams(searchParam, filter.terms);
    }

    if (filter.start) {
      searchParam = searchParam.append('start', this.formatDateParam(filter.start));
    }

    if (filter.end) {
      searchParam = searchParam.append('end', this.formatDateParam(filter.end));
    }

    searchParam = searchParam.append('pagination.page', filter.currentPage.toString());

    searchParam = searchParam.append('pagination.size', filter.pageSize.toString());

    searchParam = searchParam.append('sorting.field', filter.sortingField);

    searchParam = searchParam.append('sorting.order', filter.sortingOrder);

    return searchParam;
  }

  private createCountedDurationCollection(
    respCountedDurationCollection: RespCountedDurationCollection): CountedDurationCollection {
    return {
      items: respCountedDurationCollection.counted_durations.map(
        respItem => this.createCountedDurationItem(respItem)),
      updated: new Date()
    };
  }

  private createCountedDurationItem(item: RespCountedDurationItem): CountedDurationItem {
    return {
      count: item.count,
      duration: item.duration
    };
  }

  private createTopErrorCollection(respTopNodeErrors: RespTopNodeErrors): TopErrorsCollection {
    return {
      items: respTopNodeErrors.errors.map(respItem => this.createErrorItem(respItem)),
      updated: new Date()
    };
  }

  private createErrorItem(item: RespErrorItem): TopErrorsItem {
    return {
      count: item.count,
      type: item.type,
      message: item.error_message
    };
  }

  private createDailyCheckInCountCollection(
    respDailyCheckInCountCollection: RespDailyCheckInCountCollection): DailyCheckInCountCollection {
    return {
      buckets: respDailyCheckInCountCollection.counts.map(respDailyCheckInCount =>
        this.createDailyCheckInCount(respDailyCheckInCount)),
      updated: new Date()
    };
  }

  private createDailyCheckInCount(
    respDailyCheckInCount: RespDailyCheckInCount): DailyCheckInCount {
    return {
      start: respDailyCheckInCount.start,
      end: respDailyCheckInCount.end,
      checkInCount: respDailyCheckInCount.count,
      total: respDailyCheckInCount.total
    };
  }

  private createNodeRunsDailyStatusCollection(
    respNodeRunsDailyStatusCollection: RespNodeRunsDailyStatusCollection):
      NodeRunsDailyStatusCollection {
      return {
        buckets: respNodeRunsDailyStatusCollection.durations,
        updated: new Date()
      };
  }

  // Returns ISO 8601 formatted date string with milliseconds removed
  private formatDateParam(date: Date): string {
    return date.toISOString().split('.')[0] + 'Z';
  }
}
