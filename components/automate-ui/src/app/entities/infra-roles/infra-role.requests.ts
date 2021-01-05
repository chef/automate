import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { RolesSuccessPayload } from './infra-role.action';
import { InfraRole } from './infra-role.model';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

@Injectable()
export class InfraRoleRequests {

  constructor(private http: HttpClient) { }

  // tslint:disable-next-line: max-line-length
  public getRoles(server_id: string, org_id: string): Observable<RolesSuccessPayload> {
    return this.http.get<RolesSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/roles`, {headers});
  }

  public getRole(server_id: string, org_id: string, name: string): Observable<InfraRole> {
    return this.http.get<InfraRole>(
        `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/roles/${name}`, {headers});
  }

  public createRole(server_id: string, org_id: string, role: InfraRole): Observable<InfraRole> {

    return this.http.post<InfraRole>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/roles`, role);
  }
}
