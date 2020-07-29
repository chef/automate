import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { ClientsSuccessPayload } from './client.action';
import { Client } from './client.model';

@Injectable()
export class ClientRequests {

  constructor(private http: HttpClient) { }

  public getClients(server_id: string, org_id: string):
    Observable<ClientsSuccessPayload> {
    return this.http.get<ClientsSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/clients`);
  }

  public getClient(server_id: string, org_id: string, name: string): Observable<Client> {
    return this.http.get<Client>(
        `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/clients/${name}`);
  }
}
