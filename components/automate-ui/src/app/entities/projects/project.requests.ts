import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';

import { environment as env } from 'environments/environment';
import { Project } from './project.model';

import {
  GetProjectsSuccessPayload, ProjectSuccessPayload
} from './project.actions';

export interface ProjectsResponse {
  projects: Project[];
}

@Injectable()
export class ProjectRequests {

  constructor(private http: HttpClient) { }

  public getProjects(unfiltered?: boolean): Observable<GetProjectsSuccessPayload> {
    const options = unfiltered
      ? {
        params: new HttpParams().set('unfiltered', 'true')
      }
      : {};

    return this.http.get<GetProjectsSuccessPayload>(`${env.auth_v2_url}/projects`, options);
  }

  public getProject(id: string): Observable<ProjectSuccessPayload> {
    return this.http.get<ProjectSuccessPayload>(`${env.auth_v2_url}/projects/${id}`);
  }

  public createProject(id: string, name: string): Observable<ProjectSuccessPayload> {
    return this.http.post<ProjectSuccessPayload>(
      `${env.auth_v2_url}/projects`,
      { id, name });
  }

  public deleteProject(id: string): Observable<{}> {
    return this.http.delete(`${env.auth_v2_url}/projects/${id}`);
  }

  public updateProject(id: string, name: string): Observable<ProjectSuccessPayload> {
    return this.http.put<ProjectSuccessPayload>(
      `${env.auth_v2_url}/projects/${id}`,
      { name });
  }
}
