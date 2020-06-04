import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { map, filter, mergeMap } from 'rxjs/operators';
import { compact, concat } from 'lodash';
import { identity } from 'lodash/fp';
import { of as observableOf, Observable } from 'rxjs';
import { environment } from '../../../environments/environment';
import { NotificationRule } from './notification_rule.model';

const NOTIFIER_URL = environment.notifier_url;
const SECRETS_URL = environment.secrets_url;

export interface NotificationRulesResponse {
  rules: NotificationRule[];
}

export interface RuleResponse {
  rule: NotificationRule;
}

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

  public createNotificationRule(rule: NotificationRule, targetUsername: string,
    targetPassword: string): Observable<RuleResponse> {

    return this.createSecret(rule, targetUsername, targetPassword)
      .pipe(mergeMap((secretId: string) => {
        rule.targetSecretId = secretId;
        return this.http.post<RuleResponse>(
          this.joinToNotifierUrl(['rules']), rule.toRequest());
      }));
  }

  public updateNotificationRule(rule: NotificationRule): Observable<RuleResponse> {
    return this.updateSecret(rule, '', '')
      .pipe(mergeMap((secretId: string) => {
        rule.targetSecretId = secretId;
        const request = rule.toRequest();

        return this.http.put<RuleResponse>(encodeURI(
          this.joinToNotifierUrl(['rules', rule.id])), request);
      }));
  }

  private updateSecret(rule: NotificationRule, targetUsername: string,
    targetPassword: string): Observable<string> {
    if ( targetUsername.length > 0 || targetPassword.length > 0 ) {
      const secret = this.newSecret(rule.targetSecretId, rule.name, targetUsername, targetPassword);

      if (rule.targetSecretId.length > 0) {
        return this.http.patch<{}>(`${SECRETS_URL}/id/${rule.targetSecretId}`, secret).pipe(
          map(() => rule.targetSecretId));
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


  public deleteNotificationRule(rule: NotificationRule): Observable<RuleResponse> {
    return this.http.delete<RuleResponse>(encodeURI(
      this.joinToNotifierUrl(['rules', rule.id])));
  }

  private createSecret(rule: NotificationRule, targetUsername: string,
    targetPassword: string): Observable<string> {
    if ( targetUsername.length > 0 || targetPassword.length > 0 ) {
      const secret = this.newSecret('', rule.name, targetUsername, targetPassword);

      return this.http.post<SecretId>(`${SECRETS_URL}`, secret)
        .pipe(map(secretId => secretId.id));
    } else {
      return observableOf('');
    }
  }

  public testHookWithUsernamePassword(url: string,
    username: string, password: string): Observable<Object> {
    return this.http.post(encodeURI(
      this.joinToNotifierUrl(['webhook'])), { url, 'username_password': {username, password} });
  }

  public testHookWithNoCreds(url: string): Observable<Object> {
    return this.http.post(encodeURI(
      this.joinToNotifierUrl(['webhook'])), { url, 'none': {}});
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
