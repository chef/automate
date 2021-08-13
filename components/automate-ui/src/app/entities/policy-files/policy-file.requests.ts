import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { PolicyFilesSuccessPayload, PolicyGroupSuccessPayload } from './policy-file.action';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';
import { PolicyFile } from './policy-file.model';

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

  public getPolicyFile(server_id: string, org_id: string, name: string, revision: string):
  Observable<PolicyFile> {
    return this.http.get<PolicyFile>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/policyfiles/${name}?revision_id=${revision}`, {headers});
  }

  public getPolicyGroup(server_id: string, org_id: string, name: string):
    Observable<PolicyGroupSuccessPayload> {
    return this.http.get<PolicyGroupSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/policygroups/${name}`, {headers});
  }
}
