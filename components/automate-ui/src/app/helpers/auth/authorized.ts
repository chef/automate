import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';
import { map as rxjsMap, filter, debounceTime } from 'rxjs/operators';
import {
  isArray,
  isEmpty,
  identity,
  every,
  get,
  concat,
  some,
  has
} from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { IndexedEntities } from 'app/entities/entities';
import {
  GetSomeUserPerms, UserPermsPayload, GetUserParamPerms, UserPermsParameterizedPayload
} from 'app/entities/userperms/userperms.actions';
import { UserPermEntity } from 'app/entities/userperms/userperms.entity';
import { allPerms } from 'app/entities/userperms/userperms.selectors';

export interface CheckObj {
  endpoint: string;
  inflatedEndpoint?: string;
  verb: string;
  paramList?: string | string[];
}

export class AuthorizedChecker {

  static DebounceTime = 1; // milliseconds
  public isAuthorized$: Observable<boolean>;
  private placeholderRE = /\{[^{]+\}/;
  private allOf: CheckObj[] = [];
  private anyOf: CheckObj[] = [];

  constructor(private store: Store<NgrxStateAtom>) {
    this.isAuthorized$ =
      this.store.select(allPerms).pipe(
        debounceTime(AuthorizedChecker.DebounceTime),
        filter(perms => !isEmpty(perms)),
        filter(() => !isEmpty(this.allOf) || !isEmpty(this.anyOf)),
        filter(perms => this.permsPopulated(perms)),
        rxjsMap(perms => this.evalPerms(perms)));
  }

  public setPermissions(allOf: CheckObj[], anyOf: CheckObj[]): void {
    // store these for handling the async responses
    this.allOf = this.inflate(allOf);
    this.anyOf = this.inflate(anyOf);

    // dispatch requests for the specified permissions
    const nonParameterizedEndpoints = this.toUserPermsPayload(allOf, anyOf);
    if (nonParameterizedEndpoints.paths.length > 0) {
      this.store.dispatch(new GetSomeUserPerms(nonParameterizedEndpoints));
    }
    const payloads = this.toUserPermsParamPayload(allOf, anyOf);
    payloads.forEach(payload =>
      this.store.dispatch(new GetUserParamPerms(payload)));
  }

  private inflate(inputs: CheckObj[]): CheckObj[] {
    inputs.forEach(check => {
      if (this.placeholderRE.test(check.endpoint)) {
        let newEndpoint = check.endpoint;
        const list = isArray(check.paramList) ? check.paramList : [check.paramList];
        list.forEach(val => newEndpoint = newEndpoint.replace(this.placeholderRE, val));
        check.inflatedEndpoint = newEndpoint;
      }
    });
    return inputs;
  }

  // For a parameterized endpoint, we need to check the inflated endpoint--
  // with parameters filled in--so we know permissions for the endpoint with particular values.
  private permsPopulated(perms: IndexedEntities<UserPermEntity>): boolean {
    return every(
      (check: CheckObj) => has(check.inflatedEndpoint || check.endpoint, perms),
      concat(this.allOf, this.anyOf));
  }

  private toUserPermsPayload(allOf: CheckObj[], anyOf: CheckObj[]): UserPermsPayload {
    return {
      paths: concat(allOf, anyOf)
        .filter((check: CheckObj) => !(this.placeholderRE.test(check.endpoint)))
        .map((check: CheckObj) => check.endpoint)
    };
  }

  private toUserPermsParamPayload(allOf: CheckObj[], anyOf: CheckObj[]):
    UserPermsParameterizedPayload[] {
    // parameterized introspection using POST body not yet surfaced so this array is always empty.
    const parameters: string[] = [];

    return concat(allOf, anyOf)
      .filter((check: CheckObj) => this.placeholderRE.test(check.endpoint))
      .map((check: CheckObj) =>
        ({ path: this.fillPlaceholders(check.endpoint, check.paramList), parameters })
      );
  }

  public evalPerms(perms: IndexedEntities<UserPermEntity>): boolean {
    return every(
      identity, [this.checkAllOf(perms, this.allOf), this.checkAnyOf(perms, this.anyOf)]);
  }

  private checkAllOf(perms: IndexedEntities<UserPermEntity>, allOf: CheckObj[]): boolean {
    return every(({ endpoint, verb, paramList }: CheckObj) =>
      get([this.fillPlaceholders(endpoint, paramList), verb], perms), allOf);
  }

  private checkAnyOf(perms: IndexedEntities<UserPermEntity>, anyOf: CheckObj[]): boolean {
    return isEmpty(anyOf) ||
      some(({ endpoint, verb, paramList }: CheckObj) =>
        get([this.fillPlaceholders(endpoint, paramList), verb], perms), anyOf);
  }

  private fillPlaceholders(path: string, parameters: string | string[]): string {
    const normalizedList = (parameters instanceof Array) ? parameters : [parameters];

    let expandedPath = path;
    normalizedList.forEach(p => {
        expandedPath = expandedPath.replace(this.placeholderRE, p);
    });
    return expandedPath;
  }
}
