import {of as observableOf,  Observable } from 'rxjs';

import { map, mergeMap, filter } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { compact, concat } from 'lodash';

import { environment } from '../../../environments/environment';
import { Rule } from '../../pages/notifications/rule';

const NOTIFIER_URL = environment.notifier_url;
const SECRETS_URL = environment.secrets_url;

export interface RuleResponse {
  rule: object;
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
export class RulesService {
  constructor(private httpClient: HttpClient) { }

  public fetchRules(): Observable<Rule[]> {
    return this.httpClient.get(this.joinToNotifierUrl(['rules'])).pipe(
      map((res: Object) => res['rules']),
      filter(rulesJson => rulesJson),
      map(rulesJson =>
        rulesJson.map(rule => Rule.fromResponse(rule))
      ));
  }

  public fetchRule(id: string): Observable<Rule> {
    return this.httpClient.get<RuleResponse>(this.joinToNotifierUrl(['rules', id ])).pipe(
      map((rulesJson: RuleResponse) => Rule.fromResponse(rulesJson.rule)));
  }

  public createRule(rule: Rule, targetUsername: string,
    targetPassword: string): Observable<RuleResponse> {

    return this.createSecret(rule, targetUsername, targetPassword)
      .pipe(mergeMap((secretId: string) => {
        rule.targetSecretId = secretId;
        return this.httpClient.post<RuleResponse>(
          this.joinToNotifierUrl(['rules']), rule.toRequest());
      }));
  }

  public deleteRule(rule: Rule): Observable<RuleResponse> {
    return this.httpClient.delete<RuleResponse>(encodeURI(
      this.joinToNotifierUrl(['rules', rule.id])));
  }

  public editRule(ruleId: string, rule: Rule, targetUsername: string,
    targetPassword: string): Observable<RuleResponse> {
    return this.updateSecret(rule, targetUsername, targetPassword)
      .pipe(mergeMap((secreteId: string) => {
        rule.targetSecretId = secreteId;
        const response: any = rule.toRequest();
        // special case for authz handling - id needs to be in top level of request params
        response.id = ruleId;

        return this.httpClient.put<RuleResponse>(encodeURI(
          this.joinToNotifierUrl(['rules', ruleId])), response);
      }));
  }

  public testHookWithUsernamePassword(url: string,
    username: string, password: string): Observable<Object> {
    return this.httpClient.post(encodeURI(
      this.joinToNotifierUrl(['webhook'])), { url, 'username_password': {username, password} });
  }

  public testHookWithSecretId(url: string, secretId: string): Observable<Object> {
    return this.httpClient.post(encodeURI(
      this.joinToNotifierUrl(['webhook'])), { url, 'secret_id': { 'id': secretId } });
  }

  public testHookWithNoCreds(url: string): Observable<Object> {
    return this.httpClient.post(encodeURI(
      this.joinToNotifierUrl(['webhook'])), { url, 'none': {}});
  }

  private createSecret(rule: Rule, targetUsername: string,
    targetPassword: string): Observable<string> {
    if ( targetUsername.length > 0 || targetPassword.length > 0 ) {
      const secret = this.newSecret('', rule.name, targetUsername, targetPassword);

      return this.httpClient.post<SecretId>(`${SECRETS_URL}`, secret)
        .pipe(map(secretId => secretId.id));
    } else {
      return observableOf('');
    }
  }

  private updateSecret(rule: Rule, targetUsername: string,
    targetPassword: string): Observable<string> {
    if ( targetUsername.length > 0 || targetPassword.length > 0 ) {
      const secret = this.newSecret(rule.targetSecretId, rule.name, targetUsername, targetPassword);

      if (rule.targetSecretId.length > 0) {
        return this.httpClient.patch<any>(`${SECRETS_URL}/id/${rule.targetSecretId}`, secret).pipe(
          map(_d => rule.targetSecretId));
      } else {
        return this.httpClient.post<SecretId>(`${SECRETS_URL}`, secret).pipe(
          map(secretId => secretId.id));
      }
    } else {
      return observableOf(rule.targetSecretId);
    }
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

  // Generate an notifier url from a list of words.
  // falsey values; false, null, 0, "", undefined, and NaN are ignored
  // example: ['foo', 'bar', null, 'baz'] == '${NOTIFIERURL}/foo/bar/baz'
  private joinToNotifierUrl(words: string[]): string {
    return compact(concat([NOTIFIER_URL], words)).join('/');
  }
}
