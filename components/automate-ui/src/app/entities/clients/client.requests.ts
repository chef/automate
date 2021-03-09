import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { ClientsPayload, CreateClientPayload } from './client.action';
import { Client } from './client.model';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

export interface ClientsResponse {
  clients: Client[];
  total: number;
}

export interface CreateClientResponse {
  name: string;
  client_key: {
    name: string,
    public_key: string,
    expiration_date: string,
    private_key: string
  };
}

@Injectable()
export class ClientRequests {

  constructor(private http: HttpClient) { }

  public getClients(payload: ClientsPayload)
  : Observable<ClientsResponse> {
    const wildCardSearch = '*';
    const target = payload.clientName !== '' ?
     'name:' + wildCardSearch + payload.clientName : wildCardSearch + ':';
    const nameTarget = target + wildCardSearch;
    const currentPage = payload.page - 1;

    const params = `search_query.q=${nameTarget}&search_query.page=${currentPage}&search_query.per_page=${payload.per_page}`;
    const url = `${env.infra_proxy_url}/servers/${payload.server_id}/orgs/${payload.org_id}/clients?${params}`;
    return this.http.get<ClientsResponse>(url, {headers});
  }

  public getClient(server_id: string, org_id: string, name: string): Observable<Client> {
    return this.http.get<Client>(
        `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/clients/${name}`, {headers});
  }

  public createClient(payload: CreateClientPayload): Observable<CreateClientResponse> {
    return this.http.post<CreateClientResponse>(
      `${env.infra_proxy_url}/servers/${payload.server_id}/orgs/${payload.org_id}/clients`,
      payload);
  }

  public deleteClient(server_id: string, org_id: string, name: string): Observable<{}> {
    return this.http.delete(`${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/clients/${name}`,
    {headers});
  }

  public resetKeyClient(server_id: string, org_id: string, name: string): Observable<{}> {
    return this.http.put(`${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/clients/${name}/reset`,
    {headers});
  }
}
