import { of as observableOf, Subscription } from 'rxjs';

import { catchError, map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient, HttpErrorResponse } from '@angular/common/http';

import { NodeUsageStats, NodeUsageAckStats } from './client-runs-stats.model';

import { environment } from 'environments/environment';
const CLIENT_RUNS_STATS_URL = environment.client_runs_stats_url;

@Injectable()
export class ClientRunsStatsService {

  private clientRunsStatsSubscription: Subscription;
  constructor(private httpClient: HttpClient) {}

  getClientRunsStats(): Promise<NodeUsageStats>  {
    let resolver;
    const promise = new Promise<NodeUsageStats>((resolve) => {
      resolver = resolve;
    });
    this.clientRunsStatsSubscription = this.fetchNodeUsageStats().subscribe((data) => {
        if (this.clientRunsStatsSubscription) {
            this.clientRunsStatsSubscription.unsubscribe();
        }
        resolver(data);
    });
    return promise;
  }

  fetchNodeUsageStats() {
    const url = `${CLIENT_RUNS_STATS_URL}` + '/nodes/count';
    return this.httpClient.get(url).pipe(
      map((res) => {
          if (res && res['days_since_last_post'] && res['node_cnt']) {
            return res;
          }
          return res;
      }),
      catchError(_err => observableOf('')));
  }

  sendAcknowledgement(nodeUsageAckStats: NodeUsageAckStats) {
    const url = `${CLIENT_RUNS_STATS_URL}` + '/nodes/count/updated';
    let resolver;
    const promise = new Promise((resolve) => {
      resolver = resolve;
    });
    const nodeUsageAckStatsSubscription = this.httpClient.put(url, nodeUsageAckStats)
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
