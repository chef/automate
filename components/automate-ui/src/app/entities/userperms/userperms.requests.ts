import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { environment as env } from '../../../environments/environment';
import { IndexedEntities } from '../entities';
import { UserPermEntity } from './userperms.entity';
import { UserPermsPayload, UserPermsParameterizedPayload } from './userperms.actions';

export interface UserPermsResponse {
  endpoints: IndexedEntities<UserPermEntity>;
}

@Injectable()
export class UserPermsRequests {

  constructor(private http: HttpClient) {}

  // Returns data for all *non-parameterized* endpoints
  // (i.e. endpoints like "/iam/v2/teams" but not "/auth/users/{email}").
  public fetchAll(): Observable<UserPermsResponse> {
    const url = `${env.auth_url}/introspect`;
    return this.http.get<UserPermsResponse>(url);
  }

  // Returns data for a specific set of non-parameterized endpoints.
  public fetchSome(payload: UserPermsPayload): Observable<UserPermsResponse> {
    const url = `${env.auth_url}/introspect_some`;
    return this.http.post<UserPermsResponse>(url, payload);
  }

  // Returns data for a parameterized endpoint.
  public fetchParameterized(payload: UserPermsParameterizedPayload): Observable<UserPermsResponse> {
    const url = `${env.auth_url}/introspect`;
    return this.http.post<UserPermsResponse>(url, payload);
  }
}
