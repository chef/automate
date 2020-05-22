import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { EnvironmentsSuccessPayload } from './environment.action';
import { Environment } from './environment.model';

@Injectable()
export class EnvironmentRequests {

  constructor(private http: HttpClient) { }

  // tslint:disable-next-line: max-line-length
  public getEnvironments(server_id: string, org_id: string):
    Observable<EnvironmentsSuccessPayload> {
    return this.http.get<EnvironmentsSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/environments`);
  }

  public getEnvironment(server_id: string, org_id: string, name: string): Observable<Environment> {
    return this.http.get<Environment>(
        `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/environments/${name}`);
  }
}
