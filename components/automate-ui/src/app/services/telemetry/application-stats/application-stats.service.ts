import { of as observableOf, Subscription } from 'rxjs';

import { catchError, map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient, HttpErrorResponse } from '@angular/common/http';

import { ApplicationUsageStats, ApplicationUsageAckStats } from './application-stats.model';

import { environment } from 'environments/environment';
const APPLICATIONS_STATS_URL = environment.applications_url;

@Injectable()
export class ApplicationStatsService {

  private applicationStatsSubscription: Subscription;
  constructor(private httpClient: HttpClient) {}

  getApplicationStats(): Promise<ApplicationUsageStats>  {
    let resolver;
    const promise = new Promise<ApplicationUsageStats>((resolve) => {
      resolver = resolve;
    });
    this.applicationStatsSubscription = this.fetchApplicationUsageStats().subscribe((data) => {
        if (this.applicationStatsSubscription) {
            this.applicationStatsSubscription.unsubscribe();
        }
        resolver(data);
    });
    return promise;
  }

  fetchApplicationUsageStats() {
    const url = `${APPLICATIONS_STATS_URL}` + '/telemetry/services/count';
    return this.httpClient.get(url).pipe(
      map((res) => {
          if (res && res['total_services'] && res['days_since_last_post']) {
            return res;
          }
          return res;
      }),
      catchError(_err => observableOf('')));
  }

  // tslint:disable-next-line:no-shadowed-variable
  sendAcknowledgement(ApplicationUsageAckStats: ApplicationUsageAckStats) {
    const url = `${APPLICATIONS_STATS_URL}` + '/telemetry/services/count/updated';
    let resolver;
    const promise = new Promise((resolve) => {
      resolver = resolve;
    });
    const nodeUsageAckStatsSubscription = this.httpClient.put(url, ApplicationUsageAckStats)
    .subscribe(() => {
      if (nodeUsageAckStatsSubscription) {
        nodeUsageAckStatsSubscription.unsubscribe();
      }
      resolver('success');
    },
    ({ status, error: { message } }: HttpErrorResponse) => {
      console.log(`Error emitting Ack event: ${status} - ${message}`);
      resolver('error');
    });
    return promise;
  }

}
