import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { compact, concat } from 'lodash';

import { environment } from '../../../environments/environment';
import { Rule } from '../../pages/notifications/rule';

const NOTIFIER_URL = environment.notifier_url;

export interface RuleResponse {
  rule: object;
}

@Injectable()
export class NotificationRuleRequests {

  constructor(private http: HttpClient) { }

  public deleteNotificationRule(rule: Rule): Observable<RuleResponse> {
    return this.http.delete<RuleResponse>(encodeURI(
      this.joinToNotifierUrl(['rules', rule.id])));
  }

  private joinToNotifierUrl(words: string[]): string {
    return compact(concat([NOTIFIER_URL], words)).join('/');
  }
}
