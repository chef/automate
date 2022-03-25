import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { UsersSuccessPayload, ResetKeySuccessPayload } from './org-users.action';

@Injectable()
export class OrgUserRequests {

  constructor(private http: HttpClient) { }

  public OrgUserRequests(server_id: string, org_id: string)
  : Observable<UsersSuccessPayload> {
    const url = `${env.infra_proxy_url}/servers/${server_id}/org/${org_id}/users`;
    return this.http.get<UsersSuccessPayload>(url);
  }

  public resetUserKeyRequests(server_id: string, org_id: string, name: string)
  : Observable<ResetKeySuccessPayload> {
    const url = `${env.infra_proxy_url}/servers/${server_id}/org/${org_id}/users/${name}`;
    return this.http.get<ResetKeySuccessPayload>(url);
  }
  
}
