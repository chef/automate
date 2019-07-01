import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
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

  rules: Rule[] = [
    // {
    //   id: 'rule-1',
    //   project_id: 'project-1',
    //   name: 'Rule 1',
    //   type: 'NODE',
    //   edits: 'staging',
    //   conditions: [
    //       {
    //       attribute: 'CHEF_SERVER',
    //       values: ['adad','asdasd'],
    //       operator: 'MEMBER_OF'
    //     }
    //   ]
    // }
  ];

  constructor(private http: HttpClient) { }

  public getRules(project_id: string): Observable<GetRulesSuccessPayload> {
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
    rule.id = `rule-${this.rules.length}`;
    rule.project_id = project_id;
    rule.edits = 'staging';
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
