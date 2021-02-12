import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { EnvironmentsSuccessPayload, EnvironmentSearchPayload } from './environment.action';
import { Environment } from './environment.model';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

export interface EnvironmentSearchResponse {
  environments: any[];
  total: number;
}

@Injectable()
export class EnvironmentRequests {

  constructor(private http: HttpClient) { }

  // tslint:disable-next-line: max-line-length
  public getEnvironments(server_id: string, org_id: string):
    Observable<EnvironmentsSuccessPayload> {
    return this.http.get<EnvironmentsSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/environments`, {headers});
  }

  public getEnvironment(server_id: string, org_id: string, name: string): Observable<Environment> {
    return this.http.get<Environment>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/environments/${name}`, {headers});
  }

  public getEnvironmentSearch(payload: EnvironmentSearchPayload)
  : Observable<EnvironmentSearchResponse> {
    return this.http.get<EnvironmentSearchResponse>(
      `${env.infra_proxy_url}/servers/${payload.server_id}/orgs/${payload.org_id}/environments?search_query.q=name:*${payload.environmentName}*`,
      {headers}
    );
  }
}
