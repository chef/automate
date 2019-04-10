import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { environment as env } from 'environments/environment';
import { Policy } from './policy.model';

export interface IamVersionResponse {
  version: {
    major: 'V1' | 'V2';
    minor: 'V0' | 'V1';
  };
}

export interface PoliciesResponse {
  policies: Policy[];
}

export interface PolicyResponse {
  policy: Policy;
}

export interface MembersResponse {
  members: string[];
}

@Injectable()
export class PolicyRequests {

  constructor(private http: HttpClient) { }

  public getIamVersion(): Observable<IamVersionResponse> {
    return this.http.get<IamVersionResponse>(`${env.auth_v2_url}/policy_version`);
  }

  public getPolicies(): Observable<PoliciesResponse> {
    return this.http.get<PoliciesResponse>(`${env.auth_v2_url}/policies`);
  }

  public getPolicy(id: string): Observable<PolicyResponse> {
    return this.http.get<PolicyResponse>(`${env.auth_v2_url}/policies/${id}`);
  }

  public removePolicyMembers(id: string, members: string[]): Observable<MembersResponse> {
    return this.http.post<MembersResponse>(`${env.auth_v2_url}/policies/${id}/members:remove`,
      {'members': members});
  }

  public addPolicyMembers(id: string, members: string[]): Observable<MembersResponse> {
    return this.http.post<MembersResponse>(`${env.auth_v2_url}/policies/${id}/members:add`,
      {'members': members});
  }

  public deletePolicy(id: string): Observable<{}> {
    return this.http.delete(`${env.auth_v2_url}/policies/${id}`);
  }
}
