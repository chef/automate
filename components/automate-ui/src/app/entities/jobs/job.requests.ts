import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { environment as env } from '../../../environments/environment';
import { Job } from './job.model';

export interface JobsResponse {
  jobs: Job[];
  total: number;
}

export type JobGetResponse = Job;

export interface JobCreateResponse {
  id: string;
}

export interface JobUpdateResponse {
  id: string;
}

@Injectable()
export class JobRequests {

  constructor(private http: HttpClient) {}

  public fetchJobs(params): Observable<JobsResponse> {
    return this.http.post<JobsResponse>(`${env.compliance_url}/scanner/jobs/search`, params);
  }

  public jobGet(params): Observable<JobGetResponse> {
    return this.http.get<JobGetResponse>(`${env.compliance_url}/scanner/jobs/id/${params.id}`);
  }

  public jobCreate(params): Observable<JobCreateResponse> {
    return this.http.post<JobCreateResponse>(`${env.compliance_url}/scanner/jobs`, params);
  }

  public jobUpdate(params): Observable<JobUpdateResponse> {
    return this.http
      .put<JobUpdateResponse>(`${env.compliance_url}/scanner/jobs/id/${params.id}`, params);
  }

}
