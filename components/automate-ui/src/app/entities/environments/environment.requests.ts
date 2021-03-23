import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { GetEnvironmentsPayload } from './environment.action';
import { Environment } from './environment.model';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

export interface GetEnvironmentsResponse {
  environments: any[];
  total: number;
}

export interface EnvironmentResponse {
  environment: Environment;
}

@Injectable()
export class EnvironmentRequests {

  constructor(private http: HttpClient) { }

  // tslint:disable-next-line: max-line-length
  public getEnvironment(server_id: string, org_id: string, name: string): Observable<Environment> {
    return this.http.get<Environment>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/environments/${name}`, {headers});
  }

  public getEnvironments(payload: GetEnvironmentsPayload)
  : Observable<GetEnvironmentsResponse> {
    const wildCardSearch = '*';
    const target = payload.environmentName !== '' ?
     'name:' + wildCardSearch + payload.environmentName : wildCardSearch + ':';
    const nameTarget = target + wildCardSearch;
    const currentPage = payload.page - 1;

    const params = `search_query.q=${nameTarget}&search_query.page=${currentPage}&search_query.per_page=${payload.per_page}`;
    const url = `${env.infra_proxy_url}/servers/${payload.server_id}/orgs/${payload.org_id}/environments?${params}`;
    return this.http.get<GetEnvironmentsResponse>(url, {headers});
  }

  public createEnvironment(environment: Environment):
    Observable<EnvironmentResponse> {

    return this.http.post<EnvironmentResponse>(
      `${env.infra_proxy_url}/servers/${environment.server_id}/orgs/${environment.org_id}/environments`, environment);
  }

  public deleteEnvironment(server_id: string, org_id: string, name: string): Observable<{}> {
    return this.http.delete(`${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/environments/${name}`,
    {headers});
  }

  public updateEnvironment(environment: Environment): Observable<Environment> {
    return this.http.put<Environment>(
      `${env.infra_proxy_url}/servers/${environment.server_id}/orgs/${environment.org_id}/environments/${environment.name}`, environment);
  }
}
