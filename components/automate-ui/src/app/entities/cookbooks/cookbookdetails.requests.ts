import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
// import { CookbookDetailsSuccessPayload } from './cookbookdetails.actions';
import { CookbookDetails } from './cookbookdetails.model';

@Injectable()
export class CookbookDetailsRequests {

  constructor(private http: HttpClient) { }

  public getCookbookDetailsForVersion(
    server_id: string, org_id: string, cookbook_name: string, cookbook_version: string):
    Observable<CookbookDetails> {
    return this.http.get<CookbookDetails>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/cookbooks/${cookbook_name}/${cookbook_version}`);
  }

}
