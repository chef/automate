import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { EnvironmentsSuccessPayload } from './environment.action';

@Injectable()
export class EnvironmentRequests {

  constructor(private http: HttpClient) { }

  // tslint:disable-next-line: max-line-length
  public getEnvironments(server_id: string, org_id: string):
    Observable<EnvironmentsSuccessPayload> {
    return this.http.get<EnvironmentsSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/environments`);
  }
}
