import { map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { DailyCheckInCountCollection, DailyCheckInCount } from './desktop.model';

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

@Injectable()
export class DesktopRequests {

  constructor(private http: HttpClient) { }

  public getDailyCheckInCountCollection(daysAgo: number): Observable<DailyCheckInCountCollection> {
    return this.http.get<RespDailyCheckInCountCollection>(
      `${CONFIG_MGMT_URL}/stats/checkin_counts_timeseries?days_ago=${daysAgo}`)
      .pipe(map(respDailyCheckInCountCollection =>
        this.createDailyCheckInCountCollection(respDailyCheckInCountCollection)));
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
