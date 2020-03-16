import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';

import {
  RolesSuccessPayload, RoleSuccessPayload
} from './infra-role.action';

@Injectable()
export class InfraRoleRequests {

  constructor(private http: HttpClient) { }

  // tslint:disable-next-line: max-line-length
  public getRolesForOrgs(server_id: string, org_id: string): Observable<RolesSuccessPayload> {
    return this.http.get<RolesSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/roles`);
  }


  public getRole(server_id: string, org_id: string, name: string): Observable<RoleSuccessPayload> {
    return this.http.get<RoleSuccessPayload>(
        `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/roles/${name}`);
    }


}
