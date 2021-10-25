import { of as observableOf, Subscription } from 'rxjs';

import { catchError, map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient, HttpErrorResponse } from '@angular/common/http';

import { NodeUsageStats, NodeUsageAckStats } from './compliance-stats.model';

import { environment } from 'environments/environment';
const COMPLIANCE_STATS_URL = environment.compliance_stats_url;

@Injectable()
export class ComplianceStatsService {

  private complianceStatsSubscription: Subscription;
  constructor(private httpClient: HttpClient) {}

  getComplianceStats(): Promise<NodeUsageStats>  {
    let resolver;
    const promise = new Promise<NodeUsageStats>((resolve) => {
      resolver = resolve;
    });
    this.complianceStatsSubscription = this.fetchNodeUsageStats().subscribe((data) => {
        if (this.complianceStatsSubscription) {
            this.complianceStatsSubscription.unsubscribe();
        }
        resolver(data);
    });
    return promise;
  }

  fetchNodeUsageStats() {
    const url = `${COMPLIANCE_STATS_URL}` + '/nodes/count';
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
    const url = `${COMPLIANCE_STATS_URL}` + '/nodes/count/updated';
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
