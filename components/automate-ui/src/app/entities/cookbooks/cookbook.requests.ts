import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';

import {
  CookbooksSuccessPayload, CookbookSuccessPayload
} from './cookbook.actions';

@Injectable()
export class CookbookRequests {

  constructor(private http: HttpClient) { }

  // tslint:disable-next-line: max-line-length
  public getCookbooksForServer(server_id: string, org_id: string): Observable<CookbooksSuccessPayload> {
    return this.http.get<CookbooksSuccessPayload>(
      `${env.gateway_url}/infra_proxy/servers/${server_id}/orgs/${org_id}/cookbooks`);
  }

  // tslint:disable-next-line: max-line-length
  public getCookbook(server_id: string, org_id: string, id: string): Observable<CookbookSuccessPayload> {
    return this.http.get<CookbookSuccessPayload>(
      `${env.gateway_url}/infra_proxy/servers/${server_id}/orgs/${org_id}/${id}`);
  }

}
