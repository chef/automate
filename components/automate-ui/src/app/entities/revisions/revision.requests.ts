import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { RevisionsSuccessPayload } from './revision.action';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

@Injectable()
export class RevisionRequests {

  constructor(private http: HttpClient) { }

  public getRevisions(server_id: string, org_id: string, name: string):
    Observable<RevisionsSuccessPayload> {
      return this.http.get<RevisionsSuccessPayload>(
        `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/policyfiles/${name}/revisions`,
        {headers});
  }
}
