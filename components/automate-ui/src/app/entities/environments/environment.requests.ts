import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { EnvironmentGetAllPayload } from './environment.action';
import { Environment } from './environment.model';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

export interface EnvironmentGetAllResponse {
  environments: any[];
  total: number;
}

@Injectable()
export class EnvironmentRequests {

  constructor(private http: HttpClient) { }

  // tslint:disable-next-line: max-line-length
  public getEnvironment(server_id: string, org_id: string, name: string): Observable<Environment> {
    return this.http.get<Environment>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/environments/${name}`, {headers});
  }

  public getEnvironments(payload: EnvironmentGetAllPayload)
  : Observable<EnvironmentGetAllResponse> {
    const wildCardSearch = '*';
    const target = payload.environmentName !== '' ?
     'name:' + wildCardSearch + payload.environmentName : wildCardSearch + ':';
    const nameTarget = target + wildCardSearch;
    const currentPage = payload.page - 1;

     const params = `search_query.q=${nameTarget}&search_query.page=${currentPage}&search_query.per_page=${payload.per_page}`;
    const url = `${env.infra_proxy_url}/servers/${payload.server_id}/orgs/${payload.org_id}/environments?${params}`;
    return this.http.get<EnvironmentGetAllResponse>(url, {headers});    
  }
}
