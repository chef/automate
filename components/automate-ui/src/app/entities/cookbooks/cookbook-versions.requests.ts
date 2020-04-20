import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
// import { CookbookVersionsSuccessPayload } from './cookbookversions.actions';
import { CookbookVersions } from './cookbookversions.model';

@Injectable()
export class CookbookVersionsRequests {

  constructor(private http: HttpClient) { }

  public getCookbookVersions(server_id: string, org_id: string, cookbook_name: string):
  Observable<CookbookVersions> {
    return this.http.get<CookbookVersions>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/cookbooks/${cookbook_name}`);
  }

}
