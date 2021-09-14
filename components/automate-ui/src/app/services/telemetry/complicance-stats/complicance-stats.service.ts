import { of as observableOf,  Observable, Subscription } from 'rxjs';

import { catchError, map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';

import { UnfilteredStats } from './complicance-stats.model';

@Injectable()
export class ComplianceStatsService {

  private complianceStatsSubscription: Subscription;
  constructor(private httpClient: HttpClient) {}

  getComplianceStats(): Promise<UnfilteredStats>  {
    let resolver;
    const promise = new Promise<UnfilteredStats>((resolve) => {
      resolver = resolve;
    });
    this.complianceStatsSubscription = this.fetchUnfilteredStats().subscribe((data) => {
        if (this.complianceStatsSubscription) {
            this.complianceStatsSubscription.unsubscribe();
        }
        resolver(data);
    });
    return promise;
  }

  fetchUnfilteredStats(): Observable<UnfilteredStats> {
    const url = '/api/v0/compliance/reporting/stats/summary';
    return this.httpClient.post(url, {include_unfiltered: true}).pipe(
      map((res) => {
          if (res && res['report_summary'] && res['report_summary'].unfiltered_stats) {
            return res['report_summary'].unfiltered_stats;
          }
          return res;
      }),
      catchError(_err => observableOf('')));
  }
}
