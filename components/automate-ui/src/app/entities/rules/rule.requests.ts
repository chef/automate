import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { Rule } from './rule.model';

import {
  RulesSuccessPayload, RuleSuccessPayload
} from './rule.actions';

@Injectable()
export class RuleRequests {

  constructor(private http: HttpClient) { }

  public getRulesForProject(project_id: string): Observable<RulesSuccessPayload> {
    return this.http.get<RulesSuccessPayload>(
      `${env.auth_v2_url}/projects/${project_id}/rules`);
  }

  public getRule(project_id: string, id: string): Observable<RuleSuccessPayload> {
    return this.http.get<RuleSuccessPayload>(
      `${env.auth_v2_url}/projects/${project_id}/rules/${id}`);
  }

  public createRule(rule: Rule): Observable<RuleSuccessPayload> {
    return this.http.post<RuleSuccessPayload>(
      `${env.auth_v2_url}/projects/${rule.project_id}/rules`, rule);
  }

  public deleteRule(project_id: string, id: string): Observable<{}> {
    return this.http.delete(`${env.auth_v2_url}/projects/${project_id}/rules/${id}`);
  }

  public updateRule(rule: Rule): Observable<RuleSuccessPayload> {
    return this.http.put<RuleSuccessPayload>(
      `${env.auth_v2_url}/projects/${rule.project_id}/rules/${rule.id}`, rule);
  }
}
