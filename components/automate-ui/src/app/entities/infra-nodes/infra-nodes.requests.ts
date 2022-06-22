import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import {
  NodesPayload,
  NodesSuccessPayload,
  UpdateNodeEnvPayload,
  UpdateNodeTagPayload,
  UpdateNodeAttrPayload,
  PolicyGroupNodesPayload
} from './infra-nodes.actions';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';
import {
  InfraNode
} from 'app/entities/infra-nodes/infra-nodes.model';

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

  public getNode(server_id: string, org_id: string, name: string): Observable<InfraNode> {
    return this.http.get<InfraNode>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/nodes/${name}`, {headers});
  }

  public updateNode(node: InfraNode): Observable<InfraNode> {
    return this.http.put<InfraNode>(
      `${env.infra_proxy_url}/servers/${node.server_id}/orgs/${node.org_id}/nodes/${node.name}`,
      node);
  }

  public updateNodeEnvironment(node: UpdateNodeEnvPayload): Observable<UpdateNodeEnvPayload> {
    return this.http.put<UpdateNodeEnvPayload>(
      `${env.infra_proxy_url}/servers/${node.server_id}/orgs/${node.org_id}/nodes/${node.name}/environment`,
      node);
  }

  public updateNodeTags(node: UpdateNodeTagPayload): Observable<UpdateNodeTagPayload> {
    return this.http.put<UpdateNodeTagPayload>(
    `${env.infra_proxy_url}/servers/${node.server_id}/orgs/${node.org_id}/nodes/${node.name}/tags`,
      node);
  }

  public deleteNode(server_id: string, org_id: string, name: string): Observable<{}> {
    return this.http.delete(`${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/nodes/${name}`,
    {headers});
  }

  public updateNodeAttributes(node: UpdateNodeAttrPayload): Observable<UpdateNodeAttrPayload> {
    return this.http.put<UpdateNodeAttrPayload>(
    `${env.infra_proxy_url}/servers/${node.server_id}/orgs/${node.org_id}/nodes/${node.name}/attributes`,
      node);
  }

  public getPolicyGroupNodes(payload: PolicyGroupNodesPayload): Observable<NodesSuccessPayload> {
    const wildCardSearch = '*';
    const target = payload.policyGroupName !== '' ?
     'policy_group:' + wildCardSearch + payload.policyGroupName : wildCardSearch + ':';
    const nameTarget = target + wildCardSearch;
    const currentPage = payload.page - 1;
    // Add asterisk to do wildcard search
    const params = `search_query.q=${nameTarget}&search_query.page=${currentPage}&search_query.per_page=${payload.per_page}`;
    const url = `${env.infra_proxy_url}/servers/${payload.server_id}/orgs/${payload.org_id}/nodes?${params}`;

    return this.http.get<NodesSuccessPayload>(url, {headers});
  }
}
