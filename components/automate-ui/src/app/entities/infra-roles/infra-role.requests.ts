import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { RolesSuccessPayload, RolesPayload } from './infra-role.action';
import { InfraRole } from './infra-role.model';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

export interface RoleResponse {
  role: InfraRole;
}

@Injectable()
export class InfraRoleRequests {

  constructor(private http: HttpClient) { }

  // tslint:disable-next-line: max-line-length
  public getRoles(payload: RolesPayload): Observable<RolesSuccessPayload> {

    const wildCardSearch = '*';
    const target = payload.roleName !== '' ?
     'name:' + wildCardSearch + payload.roleName : wildCardSearch + ':';
    const nameTarget = target + wildCardSearch;
    const currentPage = payload.page - 1;
     // Add asterisk to do wildcard search
    const params = `search_query.q=${nameTarget}&search_query.page=${currentPage}&search_query.per_page=${payload.per_page}`;
    const url = `${env.infra_proxy_url}/servers/${payload.server_id}/orgs/${payload.org_id}/roles?${params}`;

    return this.http.get<RolesSuccessPayload>(url, {headers});
  }

  public getRole(server_id: string, org_id: string, name: string): Observable<InfraRole> {
    return this.http.get<InfraRole>(
        `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/roles/${name}`, {headers});
  }

  public createRole(role: InfraRole): Observable<RoleResponse> {

    return this.http.post<RoleResponse>(
      `${env.infra_proxy_url}/servers/${role.server_id}/orgs/${role.org_id}/roles`, role);
  }

  public deleteRole(server_id: string, org_id: string, name: string): Observable<{}> {
    return this.http.delete(`${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/roles/${name}`,
    {headers});
  }

  public updateRole(role: InfraRole): Observable<InfraRole> {
    return this.http.put<InfraRole>(
      `${env.infra_proxy_url}/servers/${role.server_id}/orgs/${role.org_id}/roles/${role.name}`,
      role);
  }
}
