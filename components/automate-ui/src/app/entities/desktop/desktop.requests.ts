import { map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient, HttpParams } from '@angular/common/http';
import { DailyCheckInCountCollection, DailyCheckInCount,
  TopErrorsCollection, TopErrorsItem,
  CountedDurationCollection, CountedDurationItem, Desktop, Filter } from './desktop.model';

import { environment } from '../../../environments/environment';
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

export interface RespDesktop {
  id: string;
  name: string;
  status: string;
  checkin: Date;
  uptime_seconds: number;
  platform: string;
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

  public getDesktops(filter: Filter): Observable<Desktop[]> {
    const url = `${CONFIG_MGMT_URL}/nodes`;

    const options = {
      params: this.buildURLSearchParams(filter)
    };

    return this.http.get<RespDesktop[]>(url, options).pipe(
      map((res) => res.map((respDesktop: RespDesktop) => this.createDesktop(respDesktop))));
  }

  public getDesktopsTotal(): Observable<number> {
    const url = `${CONFIG_MGMT_URL}/stats/node_counts`;

    return this.http.get<RespDesktopCount>(url).pipe(map((res) => res.total));
  }

  private createDesktop(respDesktop: RespDesktop): Desktop {
    return {
      id: respDesktop.id,
      name: respDesktop.name,
      status: respDesktop.status,
      checkin: respDesktop.checkin,
      uptimeSeconds: respDesktop.uptime_seconds,
      platform: respDesktop.platform,
      chefVersion: respDesktop.chef_version
    };
  }

  private buildUnknownDesktopDurationCountsParams(): HttpParams {
    let searchParam: HttpParams = new HttpParams();

    searchParam = searchParam.append('durations', '1M');
    searchParam = searchParam.append('durations', '2w');
    searchParam = searchParam.append('durations', '1w');
    searchParam = searchParam.append('durations', '3d');

    return searchParam;
  }

  private buildURLSearchParams(filter: Filter): HttpParams {
    let searchParam = new HttpParams();

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

}
