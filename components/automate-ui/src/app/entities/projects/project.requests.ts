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
    return this.http.get<GetProjectsSuccessPayload>(`${env.iam_url}/projects`);
  }

  public getProject(id: string): Observable<ProjectSuccessPayload> {
    return this.http.get<ProjectSuccessPayload>(`${env.iam_url}/projects/${id}`);
  }

  public createProject(id: string, name: string, skip_policies: boolean):
  Observable<ProjectSuccessPayload> {
    return this.http.post<ProjectSuccessPayload>(
      `${env.iam_url}/projects`,
      { id, name, skip_policies });
  }

  public deleteProject(id: string): Observable<{}> {
    return this.http.delete(`${env.iam_url}/projects/${id}`);
  }

  public updateProject(id: string, name: string): Observable<ProjectSuccessPayload> {
    return this.http.put<ProjectSuccessPayload>(
      `${env.iam_url}/projects/${id}`,
      { name });
  }

  public applyRulesStart(): Observable<{}> {
    return this.http.post<any>(`${env.iam_url}/apply-rules`, '{}');
  }

  public applyRulesStop(): Observable<{}> {
    return this.http.delete<any>(`${env.iam_url}/apply-rules`);
  }

  public getApplyRulesStatus(): Observable<GetApplyRulesStatusSuccessPayload> {
    return this.http.get<GetApplyRulesStatusSuccessPayload>(`${env.iam_url}/apply-rules`);
  }
}
