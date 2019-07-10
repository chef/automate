import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { environment as env } from 'environments/environment';
import { Project } from './project.model';

import {
  GetApplyRulesStatusSuccessPayload,
  GetProjectsSuccessPayload,
  ProjectSuccessPayload
} from './project.actions';

export interface ProjectsResponse {
  projects: Project[];
}

@Injectable()
export class ProjectRequests {

  constructor(private http: HttpClient) { }

  public getProjects(): Observable<GetProjectsSuccessPayload> {
    return this.http.get<GetProjectsSuccessPayload>(`${env.auth_v2_url}/projects`);
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

  public applyRulesStart(): Observable<{}> {
    return this.http.post<any>(`${env.auth_v2_url}/apply-rules`, '{}');
  }

  public applyRulesStop(): Observable<{}> {
    return this.http.delete<any>(`${env.auth_v2_url}/apply-rules`);
  }

  public getApplyRulesStatus(): Observable<GetApplyRulesStatusSuccessPayload> {
    return this.http.get<GetApplyRulesStatusSuccessPayload>(`${env.auth_v2_url}/apply-rules`);
  }
}
