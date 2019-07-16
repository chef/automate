import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { environment as env } from 'environments/environment';
import { Project } from 'app/entities/projects/project.model';

export interface AuthorizedProjectsResponse {
  projects: Project[];
}

@Injectable()
export class ProjectsFilterRequests {

  constructor(private http: HttpClient) { }

  fetchOptions(): Observable<AuthorizedProjectsResponse> {
    return this.http.get<AuthorizedProjectsResponse>(`${env.auth_v2_url}/introspect_projects`);
  }
}

