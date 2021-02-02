import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { ClientsSuccessPayload, ClientSearchPayload } from './client.action';
import { Client } from './client.model';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

export interface ClientSearchResponse {
  clients: any[];
  total: number;
}

@Injectable()
export class ClientRequests {

  constructor(private http: HttpClient) { }

  public getClients(server_id: string, org_id: string):
    Observable<ClientsSuccessPayload> {
    return this.http.get<ClientsSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/clients`, {headers});
  }

  public getClient(server_id: string, org_id: string, name: string): Observable<Client> {
    return this.http.get<Client>(
        `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/clients/${name}`, {headers});
  }

  public getClientSearch(payload: ClientSearchPayload)
  : Observable<ClientSearchResponse> {
    return this.http.get<ClientSearchResponse>(
      `${env.infra_proxy_url}/servers/${payload.server_id}/orgs/${payload.org_id}/clients?search_query.${payload.query}=name:*${payload.clientId}*`,
      {headers}
    );
  }
}
