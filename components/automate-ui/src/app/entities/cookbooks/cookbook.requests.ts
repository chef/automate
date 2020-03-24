import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { CookbooksSuccessPayload, CookbookDetailsSuccessPayload} from './cookbook.actions';

@Injectable()
export class CookbookRequests {

  constructor(private http: HttpClient) { }

  public getCookbooksForOrgs(server_id: string, org_id: string):
  Observable<CookbooksSuccessPayload> {
    return this.http.get<CookbooksSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/cookbooks/num_versions`);
  }

  public getCookbookDetails(
    server_id: string, org_id: string, cookbook_name: string, cookbook_version: string):
    Observable<CookbookDetailsSuccessPayload> {
    return this.http.get<CookbookDetailsSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/cookbooks/${cookbook_name}/${cookbook_version}`);
  }

}
