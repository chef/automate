import { map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient, HttpParams } from '@angular/common/http';
import { DailyCheckInCountCollection, DailyCheckInCount,
  TopErrorsCollection, TopErrorsItem,
  CountedDurationCollection, CountedDurationItem } from './desktop.model';

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

  private buildUnknownDesktopDurationCountsParams(): HttpParams {
    let searchParam: HttpParams = new HttpParams();

    searchParam = searchParam.append('durations', '1M');
    searchParam = searchParam.append('durations', '2w');
    searchParam = searchParam.append('durations', '1w');
    searchParam = searchParam.append('durations', '3d');

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
