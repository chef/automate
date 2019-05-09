import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { environment as env } from 'environments/environment';

export interface AuthorizedProjectsResponse {
    projects: string[];
  }

@Injectable()
export class ProjectsFilterRequests {

  constructor(private http: HttpClient) { }

  fetchOptions(): Observable<AuthorizedProjectsResponse[]> {
    return this.http.get<AuthorizedProjectsResponse[]>(`${env.auth_url}/introspect_projects`);
  }
}

