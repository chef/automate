import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { PolicyFilesSuccessPayload } from './policy-file.action';

@Injectable()
export class PolicyFileRequests {

  constructor(private http: HttpClient) { }

  // tslint:disable-next-line: max-line-length
  public getPolicyFiles(server_id: string, org_id: string):
    Observable<PolicyFilesSuccessPayload> {
    return this.http.get<PolicyFilesSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/policyfiles`);
  }
}
