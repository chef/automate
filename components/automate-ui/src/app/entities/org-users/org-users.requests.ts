import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { UsersSuccessPayload, ResetKeySuccessPayload } from './org-users.action';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

@Injectable()
export class OrgUserRequests {

  constructor(private http: HttpClient) { }

  //  get all org users
  public OrgUserRequests(server_id: string, org_id: string)
  : Observable<UsersSuccessPayload> {
    const url = `${env.infra_proxy_url}/servers/${server_id}/org/${org_id}/users`;
    return this.http.get<UsersSuccessPayload>(url);
  }

  // Reset user key request
  public resetUserKeyRequests(server_id: string, name: string)
  : Observable<ResetKeySuccessPayload> {
    return this.http.put<ResetKeySuccessPayload>
    (`${env.infra_proxy_url}/servers/${server_id}/user/${name}`, {headers});
  }
}
