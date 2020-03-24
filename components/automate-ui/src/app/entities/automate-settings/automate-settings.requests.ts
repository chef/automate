import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';


import {
  JobSchedulerStatus,
  RespJob,
  IngestJob,
  RespJobSchedulerStatus,
  UnfurledJob,
  JobRequestBody,
  InfraJobName,
  JobRequestComponent,
  JobCategories
} from './automate-settings.model';

import { environment } from '../../../environments/environment';

const RETENTION_URL = environment.retention_url;

@Injectable()
export class AutomateSettingsRequests {

  constructor(private http: HttpClient) {}

  // fetchJobSchedulerStatus sends an HTTP GET Request to read the status of the JobScheduler
  // living inside the ingest-service
  public fetchJobSchedulerStatus(_params): Observable<JobSchedulerStatus> {

    const url = `${RETENTION_URL}/status`;

    return this.http
      .get<RespJobSchedulerStatus>(url).pipe(
      map((res) => this.convertResponseToJobSchedulerStatus(res)));
  }

  // configureIngestJob sends an HTTP PUT Request to the provided ingest job to configure
  // it with the provided threshold and running state
  configureIngestJobs(jobs: IngestJob[]): Observable<any> {
    const url = `${RETENTION_URL}/config`;

    const body: JobRequestBody = {
      infra : {
        job_settings: [
          {
            name: 'periodic_purge_timeseries',
            purge_policies: {
              elasticsearch: []
            }
          }
        ]
      },
      compliance: {
        job_settings: [
          {
            name: 'periodic_purge',
            purge_policies: {
            elasticsearch: []
            }
          }
        ]
      },
      event_feed: {
        job_settings: [
          {
            name: 'periodic_purge',
            purge_policies: {
            elasticsearch: []
            }
          }
        ]
      }
    };

    jobs.forEach(job => {
      let thisJob: UnfurledJob;

      switch (job.name) {
        case InfraJobName.DeleteNodes:              // fallthrough
        case InfraJobName.MissingNodes:             // fallthrough
        case InfraJobName.MissingNodesForDeletion:
          thisJob = this.unfurlIngestJob(job);
          body[job.category].job_settings.push(thisJob);
          break;

        case InfraJobName.PeriodicPurgeTimeseries:  // fallthrough

        case 'periodic_purge':                      // all other nested jobs are
                                                    // contained in 'periodic_purge'
          thisJob = this.unfurlIngestJob(job, true);
          const matchedNest: JobRequestComponent =
            body[job.category].job_settings.find(item => item.name === job.name);
          matchedNest.purge_policies.elasticsearch.push(thisJob);
          break;

        default:
          break;
      }

    });

    return this.http.put<any>(url, body);
  }

  private unfurlIngestJob(job: IngestJob, nested: boolean = false): UnfurledJob {
    if (nested) {
      return {
        policy_name: job.nested_name,
        older_than_days: parseInt(job.threshold, 10),
        disabled: job.disabled
      };
    } else {
      return {
        name: job.name,
        threshold: job.threshold,
        disabled: job.disabled
      };
    }
  }


  private convertResponseToJobSchedulerStatus(
    respJobSchedulerStatus: RespJobSchedulerStatus): JobSchedulerStatus {

    const allJobs = [];

    Object.keys(respJobSchedulerStatus).forEach((category: JobCategories) => {
      if (respJobSchedulerStatus[category]) {
        const catJobs = respJobSchedulerStatus[category].jobs
          .map((respJob: RespJob) => new IngestJob(category, respJob));
        allJobs.push(...catJobs);
      }
    });

    return new JobSchedulerStatus(allJobs);
  }

}
