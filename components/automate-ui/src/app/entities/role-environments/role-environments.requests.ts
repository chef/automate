import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { RoleEnvironmentsSuccessPayload } from './role-environments.action';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

@Injectable()
export class RoleEnvironmentRequests {

  constructor(private http: HttpClient) { }

  public getRoleEnvironments(server_id: string, org_id: string, name: string):
    Observable<RoleEnvironmentsSuccessPayload> {
      return this.http.get<RoleEnvironmentsSuccessPayload>(
        `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/roles/${name}/environments`,
        {headers});
  }
}
