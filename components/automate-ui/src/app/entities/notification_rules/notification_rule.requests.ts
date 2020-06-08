import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { map, mergeMap, filter } from 'rxjs/operators';
import { compact, concat } from 'lodash';
import { identity } from 'lodash/fp';
import { of as observableOf, Observable } from 'rxjs';

import { environment } from 'environments/environment';
import { NotificationRule } from './notification_rule.model';

const NOTIFIER_URL = environment.notifier_url;
const SECRETS_URL = environment.secrets_url;

interface KVData {
  key?: string;
  value?: string;
}

interface SecretId {
  id: string;
}

interface Secret {
  id?: string;
  name: string;
  type: string;
  data: Array<KVData>;
}

export interface RuleResponse {
  rule: object;
}

export interface NotificationRulesResponse {
  rules: NotificationRule[];
}

@Injectable()
export class NotificationRuleRequests {

  constructor(private http: HttpClient) { }

  public getNotificationRules(): Observable<NotificationRule[]> {
    return this.http.get(this.joinToNotifierUrl(['rules'])).pipe(
      map((res: Object) => res['rules']),
      filter(identity),
      map(rulesJson =>
        rulesJson.map(rule => NotificationRule.fromResponse(rule))
      ));
  }

  public getNotificationRule(id: string): Observable<NotificationRule> {
    return this.http.get<RuleResponse>(this.joinToNotifierUrl(['rules', id ])).pipe(
      map((rulesJson: RuleResponse) => NotificationRule.fromResponse(rulesJson.rule)));
  }

  public updateNotificationRule(rule: NotificationRule, targetUsername: string,
    targetPassword: string): Observable<RuleResponse> {
    return this.updateSecret(rule, targetUsername, targetPassword)
      .pipe(mergeMap((secreteId: string) => {
        rule.targetSecretId = secreteId;
        const response: any = rule.toRequest();
        // special case for authz handling - id needs to be in top level of request params
        response.id = rule.id;

        return this.http.put<RuleResponse>(encodeURI(
          this.joinToNotifierUrl(['rules', rule.id])), response);
      }));
  }

  private updateSecret(rule: NotificationRule, targetUsername: string,
    targetPassword: string): Observable<string> {
    if ( targetUsername.length > 0 || targetPassword.length > 0 ) {
      const secret = this.newSecret(rule.targetSecretId, rule.name, targetUsername, targetPassword);

      if (rule.targetSecretId.length > 0) {
        return this.http.patch<any>(`${SECRETS_URL}/id/${rule.targetSecretId}`, secret).pipe(
          map(_d => rule.targetSecretId));
      } else {
        return this.http.post<SecretId>(`${SECRETS_URL}`, secret).pipe(
          map(secretId => secretId.id));
      }
    } else {
      return observableOf(rule.targetSecretId);
    }
  }

  public testNotification(url: string, secretId: string): Observable<Object> {
    return this.http.post(encodeURI(
      this.joinToNotifierUrl(['webhook'])), { url, 'secret_id': { 'id': secretId } });
  }

  private newSecret(id: string, name: string, targetUsername: string,
    targetPassword: string): Secret {
    return {
      id: id,
      name: name,
      type: 'service_now',
      data: Array<KVData>(
        {key: 'username', value: targetUsername},
        {key: 'password', value: targetPassword})
    };
  }

  private joinToNotifierUrl(words: string[]): string {
    return compact(concat([NOTIFIER_URL], words)).join('/');
  }
}
