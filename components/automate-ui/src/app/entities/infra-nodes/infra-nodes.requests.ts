import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { NodesSuccessPayload } from './infra-nodes.actions';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

@Injectable()
export class InfraNodeRequests {

  constructor(private http: HttpClient) { }

  public getNodes(server_id: string, org_id: string):
  Observable<NodesSuccessPayload> {
    const headers = new HttpHeaders().set(InterceptorSkipHeader, '');
    return this.http.get<NodesSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/nodes`, {headers});
  }

}
