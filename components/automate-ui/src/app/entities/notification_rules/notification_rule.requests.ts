import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { map, filter } from 'rxjs/operators';
import { compact, concat } from 'lodash';
import { Observable } from 'rxjs';
import { environment } from '../../../environments/environment';
import { NotificationRule } from './notification_rule.model';

const NOTIFIER_URL = environment.notifier_url;

export interface NotificationRulesResponse {
  rules: NotificationRule[];
}

export interface RuleResponse {
  rule: object;
}

@Injectable()
export class NotificationRuleRequests {

  constructor(private http: HttpClient) { }

  public getNotificationRules(): Observable<NotificationRule[]> {
    return this.http.get(this.joinToNotifierUrl(['rules'])).pipe(
      map((res: Object) => res['rules']),
      filter(rulesJson => rulesJson),
      map(rulesJson =>
        rulesJson.map(rule => NotificationRule.fromResponse(rule))
      ));
  }

  private joinToNotifierUrl(words: string[]): string {
    return compact(concat([NOTIFIER_URL], words)).join('/');
  }
}
