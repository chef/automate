import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { CookbooksSuccessPayload } from './cookbook.actions';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

@Injectable()
export class CookbookRequests {

  constructor(private http: HttpClient) { }

  public getCookbooks(server_id: string, org_id: string):
  Observable<CookbooksSuccessPayload> {
    const headers = new HttpHeaders().set(InterceptorSkipHeader, '');
    return this.http.get<CookbooksSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/cookbooks`, {headers});
  }

}
