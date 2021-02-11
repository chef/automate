import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { RolesSuccessPayload, RoleSearchPayload } from './infra-role.action';
import { InfraRole } from './infra-role.model';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

export interface RoleSearchResponse {
  roles: InfraRole[];
  total: number;
}

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

  public getRoleSearch(payload: RoleSearchPayload): Observable<RoleSearchResponse> {
    const params = `search_query.q=name:${payload.roleId}*&search_query.page=${payload.page}&search_query.per_page=${payload.per_page}`;
    const url = `${env.infra_proxy_url}/servers/${payload.server_id}/orgs/${payload.org_id}/roles?${params}`;

    return this.http.get<RoleSearchResponse>(url, {headers});
  }
}
