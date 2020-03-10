import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';


import {
  JobSchedulerStatus,
  RespJob,
  IngestJob,
  IngestJobs,
  RespJobSchedulerStatus
} from './automate-settings.model';

import { environment } from '../../../environments/environment';
import { jobSchedulerStatus } from './automate-settings.selectors';

const RETENTION_URL = environment.retention_url;

@Injectable()
export class AutomateSettingsRequests {

  constructor(private http: HttpClient) {}

  // fetchJobSchedulerStatus sends an HTTP GET Request to read the status of the JobScheduler
  // living inside the ingest-service
  public fetchJobSchedulerStatus(_params): Observable<JobSchedulerStatus> {
    // const url = `${RETENTION_URL}/nodes/status`;

    // NEW ENDPOINT FOR DATA-LIFECYCLE
    const url = '/api/v0/data-lifecycle/status';

    return this.http
      .get<RespJobSchedulerStatus>(url).pipe(
      map((res) => this.convertResponseToJobSchedulerStatus(res)));
  }

  // configureIngestJob sends an HTTP POST Request to the provided ingest job to configure
  // it with the provided threshold and running state
  configureIngestJob(job: IngestJob): Observable<any> {
    let url: string;

    console.log('configure inject Job');

    switch (job.name) {
      case IngestJobs.MissingNodes: {
        url = `${RETENTION_URL}/nodes/missing-nodes/config`;
        break;
      }
      case IngestJobs.MissingNodesForDeletion: {
        url = `${RETENTION_URL}/nodes/missing-nodes-deletion/config`;
        break;
      }
      case IngestJobs.DeleteNodes: {
        url = `${RETENTION_URL}/nodes/delete-nodes/config`;
        break;
      }
      default:
        return;
    }

    const body = {
      // March 10 - this will need full updating with new RespJob type
      'threshold': job.threshold,
      'running': job.disabled
      // We can also modify how often the job runs (every X time)
      // but we don't need that now!
      // 'every': job.every
    };

    return this.http.post<any>(url, body);
  }

  private convertResponseToJobSchedulerStatus(
    respJobSchedulerStatus: RespJobSchedulerStatus): JobSchedulerStatus {
      // THIS IS MENTAL - THIS HAS TO CHANGE
    const allJobs = {};

    Object.keys(respJobSchedulerStatus).forEach((category: string) => {
      if (respJobSchedulerStatus[category]) {
        const catJobs = respJobSchedulerStatus[category].jobs
          .map((respJob: RespJob) => new IngestJob(respJob));
        allJobs[category] = catJobs;
      }
    });

    return new JobSchedulerStatus(allJobs);
  }

}
