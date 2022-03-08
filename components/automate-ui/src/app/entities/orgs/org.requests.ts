import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { Org } from './org.model';
import {
  OrgsSuccessPayload,
  OrgSuccessPayload,
  CreateOrgPayload,
  UploadSuccessPayload,
  CancelSuccessPayload,
  PreviewSuccessPayload,
  ConfirmSuccessPayload,
  CheckUserPayload
} from './org.actions';

@Injectable()
export class OrgRequests {

  constructor(private http: HttpClient) { }

  public getOrgs(server_id: string): Observable<OrgsSuccessPayload> {
    return this.http.get<OrgsSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs`);
  }

  public getOrg(server_id: string, id: string): Observable<OrgSuccessPayload> {
    return this.http.get<OrgSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${id}`);
  }

  public createOrg(org: CreateOrgPayload): Observable<OrgSuccessPayload> {
    return this.http.post<OrgSuccessPayload>(
      `${env.infra_proxy_url}/servers/${org.server_id}/orgs`, org);
  }

  public deleteOrg(server_id: string, id: string): Observable<{}> {
    return this.http.delete(`${env.infra_proxy_url}/servers/${server_id}/orgs/${id}`);
  }

  public updateOrg(org: Org): Observable<OrgSuccessPayload> {
    return this.http.put<OrgSuccessPayload>(
      `${env.infra_proxy_url}/servers/${org.server_id}/orgs/${org.id}`, org);
  }

  public uploadZip(formData: FormData): Observable<UploadSuccessPayload> {
    return this.http.post<UploadSuccessPayload>
    (`${env.infra_proxy_url}/servers/migrations/upload`, formData);
  }

  public cancelMigration(server_id: string, migration_id: string)
    : Observable<CancelSuccessPayload> {
    return this.http.get<CancelSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/migrations/cancel_migration/${migration_id}`);
  }

  public getPreviewData(migration_id: string): Observable<PreviewSuccessPayload> {
    return this.http.get<PreviewSuccessPayload>(`
    ${env.infra_proxy_url}/servers/migrations/staged_data/${migration_id}`);
  }

  public confirmPreview(server_id: string, migration_id: string)
  : Observable<ConfirmSuccessPayload> {
    return this.http.get<ConfirmSuccessPayload>(
      `${env.infra_proxy_url}/servers/${server_id}/migrations/confirm_preview/${migration_id}`);
  }

  public checkUser(user: string): Observable<CheckUserPayload> {
    return this.http.get<CheckUserPayload>(
      `https://a2-dev.test/apis/iam/v2/users/${user}`
    );
  }
}
