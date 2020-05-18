import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { ClientsSuccessPayload } from './client.action';

@Injectable()
export class ClientRequests {

  constructor(private http: HttpClient) { }

  // tslint:disable-next-line: max-line-length
  public getClients(server_id: string, org_id: string):
    Observable<ClientsSuccessPayload> {
    return this.http.get<ClientsSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/clients`);
  }
}
