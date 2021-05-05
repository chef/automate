import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { NodesPayload, NodesSuccessPayload } from './infra-nodes.actions';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

@Injectable()
export class InfraNodeRequests {

  constructor(private http: HttpClient) { }

  public getNodes(payload: NodesPayload): Observable<NodesSuccessPayload> {

    const wildCardSearch = '*';
    const target = payload.nodeName !== '' ?
     'name:' + wildCardSearch + payload.nodeName : wildCardSearch + ':';
    const nameTarget = target + wildCardSearch;
    const currentPage = payload.page - 1;
    // Add asterisk to do wildcard search
    const params = `search_query.q=${nameTarget}&search_query.page=${currentPage}&search_query.per_page=${payload.per_page}`;
    const url = `${env.infra_proxy_url}/servers/${payload.server_id}/orgs/${payload.org_id}/nodes?${params}`;

    return this.http.get<NodesSuccessPayload>(url, {headers});
  }

  public deleteNode(server_id: string, org_id: string, name: string): Observable<{}> {
    return this.http.delete(`${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/nodes/${name}`,
    {headers});
  }

}
