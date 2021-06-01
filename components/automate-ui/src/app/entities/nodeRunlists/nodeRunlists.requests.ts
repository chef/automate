import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';
import { NodeRunlist } from './nodeRunlists.model';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

@Injectable()
export class NodeRunlistRequests {

  constructor(private http: HttpClient) { }

  public getNodeRunlist(server_id: string, org_id: string, name: string, id: string):
    Observable<NodeRunlist> {
      return this.http.get<NodeRunlist>(
        `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/node/${name}/runlist/${id}`,
        {headers});
  }
}
