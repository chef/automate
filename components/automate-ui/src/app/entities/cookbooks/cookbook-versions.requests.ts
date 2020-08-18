import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { CookbookVersions } from './cookbook-versions.model';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

@Injectable()
export class CookbookVersionsRequests {

  constructor(private http: HttpClient) { }

  public getCookbookVersions(server_id: string, org_id: string, cookbook_name: string):
  Observable<CookbookVersions> {
    const headers = new HttpHeaders().set(InterceptorSkipHeader, '');
    return this.http.get<CookbookVersions>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/cookbooks/${cookbook_name}`,
        { headers });
  }

}
