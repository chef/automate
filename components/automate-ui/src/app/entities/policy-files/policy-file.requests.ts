import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { PolicyFilesSuccessPayload } from './policy-file.action';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

@Injectable()
export class PolicyFileRequests {

  constructor(private http: HttpClient) { }

  public getPolicyFiles(server_id: string, org_id: string):
    Observable<PolicyFilesSuccessPayload> {
    return this.http.get<PolicyFilesSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/policyfiles`, {headers});
  }

  public deletePolicyFiles(server_id: string, org_id: string, name: string): Observable<{}> {
    return this.http.delete(`${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/policyfiles/${name}`,
    {headers});
  }
}
