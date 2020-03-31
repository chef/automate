import { map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { DailyCheckInCountCollection, DailyCheckInCount,
  TopErrorsCollection, TopErrorsItem } from './desktop.model';

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

  private createTopErrorCollection(respTopNodeErrors: RespTopNodeErrors): TopErrorsCollection {
    return {
      items: respTopNodeErrors.errors.map(respItem => this.createErrorItem(respItem))
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
        this.createDailyCheckInCount(respDailyCheckInCount))
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
