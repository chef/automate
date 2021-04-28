import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';
import { Runlist } from './runlists.model';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

@Injectable()
export class RunlistRequests {

  constructor(private http: HttpClient) { }

  public getRunlist(server_id: string, org_id: string, name: string, id: string):
    Observable<Runlist> {
      return this.http.get<Runlist>(
        `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/roles/${name}/runlist/${id}`,
        {headers});
  }
}
