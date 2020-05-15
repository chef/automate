import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { AdminKey } from './reset-admin-key.model';

// import {
//   AdminKeySuccessPayload
// } from './reset-admin-key.actions';

export interface AdminKeyResponse {
  adminKey: AdminKey;
}

@Injectable()
export class AdminKeyRequests {

  constructor(private http: HttpClient) { }

  public updateAdminKey(server_id: string, org_id: string, admin_Key: AdminKey):
  Observable<AdminKeyResponse> {
    return this.http.put<AdminKeyResponse>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/reset-key`, admin_Key);
  }
}
