import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { mapKeys, snakeCase } from 'lodash/fp';

import { environment as env } from 'environments/environment';
import { Server } from './server.model';
import { CreateServerPayload, ServerSuccessPayload } from './server.actions';

export interface ServersResponse {
  servers: Server[];
}

export interface ServerResponse {
  server: Server;
}

@Injectable()
export class ServerRequests {

  constructor(private http: HttpClient) { }

  public getServers(): Observable<ServersResponse> {
    return this.http.get<ServersResponse>(`${env.gateway_url}/infra_proxy/servers`);
  }

  public getServer(id: string): Observable<ServerResponse> {
    return this.http.get<ServerResponse>(`${env.gateway_url}/infra_proxy/servers/${id}`);
  }

  public createServer(serverData: CreateServerPayload): Observable<ServerResponse> {
    return this.http.post<ServerResponse>(
      `${env.gateway_url}/infra_proxy/servers`, mapKeys(snakeCase, serverData));
  }

  public updateServer(server: Server): Observable<ServerSuccessPayload> {
    return this.http.put<ServerSuccessPayload>(
      `${env.gateway_url}/infra_proxy/servers/${server.id}`, server);
  }

  public deleteServer(id: string): Observable<ServerResponse> {
    return this.http.delete<ServerResponse>(`${env.gateway_url}/infra_proxy/servers/${id}`);
  }
}
