import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { environment as env } from 'environments/environment';
import { Role } from './role.model';

export interface RolesResponse {
  roles: Role[];
}

export interface RoleResponse {
  role: Role;
}

@Injectable()
export class RoleRequests {

  constructor(private http: HttpClient) { }

  public getRoles(): Observable<RolesResponse> {
    return this.http.get<RolesResponse>(`${env.iam_url}/roles`);
  }

  public getRole(id: string): Observable<RoleResponse> {
    return this.http.get<RoleResponse>(`${env.iam_url}/roles/${id}`);
  }

  public deleteRole(id: string): Observable<RoleResponse> {
    return this.http.delete<RoleResponse>(`${env.iam_url}/roles/${id}`);
  }
}
