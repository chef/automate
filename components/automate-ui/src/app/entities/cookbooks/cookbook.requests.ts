import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';

import {
  CookbooksSuccessPayload
} from './cookbook.actions';

@Injectable()
export class CookbookRequests {

  constructor(private http: HttpClient) { }

  // tslint:disable-next-line: max-line-length
  public getCookbooksForOrgs(server_id: string, org_id: string): Observable<CookbooksSuccessPayload> {
    return this.http.get<CookbooksSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/cookbooks`);
  }

}
