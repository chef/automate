import { Injectable } from '@angular/core';
// import { HttpClient } from '@angular/common/http';
import { Observable, of } from 'rxjs';
// import { environment as env } from 'environments/environment';
import { find, filter } from 'lodash';
import { Rule } from './rule.model';

import {
  GetRulesSuccessPayload, RuleSuccessPayload
} from './rule.actions';

export interface RulesResponse {
  rules: Rule[];
}

@Injectable()
export class RuleRequests {

  // Mocked Rule data until API's are finished.
  rules: Rule[] = [];

  // TODO use http constructor when we hook up the backend
  // constructor(private http: HttpClient) { }
  constructor() { }

  public getRulesForProject(project_id: string): Observable<GetRulesSuccessPayload> {
    // return this.http.get<GetRulesSuccessPayload>(
      // `${env.auth_v2_url}/project/${project_id}/rules`);
    const rules: any = filter(this.rules, ['project_id', project_id]);
    return of({ rules: rules });
  }

  public getRule(id: string): Observable<RuleSuccessPayload> {
    // return this.http.get<RuleSuccessPayload>(`${env.auth_v2_url}/rules/${id}`);
    const rule: any = find(this.rules, ['id', id]);
    return of({ rule: rule });
  }

  public createRule(project_id, rule: Rule): Observable<RuleSuccessPayload> {
    // return this.http.post<RuleSuccessPayload>(
    //   `${env.auth_v2_url}/project/${project_id}/rules`,
    //   { rule });
    this.rules.push(rule);
    return of({ rule: rule });
  }

  public deleteRule(id: string): Observable<{}> {
    // return this.http.delete(`${env.auth_v2_url}/rules/${id}`);
    return of({ id: id });
  }

  public updateRule(rule: Rule): Observable<RuleSuccessPayload> {
    // return this.http.put<RuleSuccessPayload>(
    //   `${env.auth_v2_url}/project/${rule.project_id}/rules/${rule.id}`,
    //   { rule });
    let updatedRule: any = find(this.rules, ['id', rule.id]);
    updatedRule = rule;
    return of({ rule: updatedRule });
  }
}
