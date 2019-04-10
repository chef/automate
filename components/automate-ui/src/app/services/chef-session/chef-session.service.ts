import { Injectable } from '@angular/core';
import { isNull, isNil } from 'lodash';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot } from '@angular/router';

import { environment } from '../../../environments/environment';
import { Jwt } from 'app/helpers/jwt/jwt';

// Should never be on in production. Modify environment.ts locally
// if you wish to bypass getting a session from dex.
const USE_DEFAULT_SESSION = environment.use_default_session;

export interface ChefSessionUser {
  fullname: string;
  username: string;
  groups: Array<string>;
  id_token: string;
  telemetry_enabled?: boolean;
  uuid: string;
}

const sessionKey = 'chef-automate-user';
const HTTP_STATUS_OK = 200;
const HTTP_STATUS_UNAUTHORIZED = 401;
const XHR_DONE = 4;

// TODO 2017/11/06 sr: ChefSessionService implementing the route guard is a bit
// of a hack. Something created for the purpose of route-guarding alone would
// be better, and a good refactoring opportunity.
@Injectable()
export class ChefSessionService implements CanActivate {
  private user: ChefSessionUser;

  // Session state keys - We use session storage to save state here because
  // the application can be reinitialized multiple time during a single session.
  //// Flag to store whether or not the modal has been displayed this session.
  //// Automatically set when the modal is shown for the first time.
  MODAL_HAS_BEEN_SEEN_KEY = 'welcome-modal-seen';

  constructor() {
    // In dev mode, set a generic session so we don't
    // have to round-trip to the oidc provider (dex).
    if (USE_DEFAULT_SESSION) {
      this.setDefaultSession();
    } else {
      const minute = 60 * 1000; // try to refresh session every minute

      // Note 2017/12/13 (sr): This has to be more frequent then our token
      // expiry. However, it can be MUCH MORE frequent, as the service just
      // returns the valid token until it's expiry is soonish.
      // If we do this less often, we're increasing the chances of API requests
      // using an expired token => this will trigger a re-login.

      // TODO: figure out the proper, correct interplay between authinterceptor
      // and components/session-service and chef-session-service -- it would
      // make sense to try to refresh the session if we get a 401 from the API,
      // before giving up and calling logout().
      window.setInterval(() => {
        /* Using XMLHttpRequest instead of angular/http to avoid
           dependency hell. */
        const xhr = new XMLHttpRequest();
        xhr.onreadystatechange = this.refreshSessionCallback.bind(this);
        xhr.open('GET', '/session/refresh', true);
        xhr.responseType = 'json';
        xhr.setRequestHeader('Authorization', `Bearer ${this.id_token}`);
        xhr.send(null);
      }, minute);
    }

    this.tryInitializeSession();
  }

  refreshSessionCallback(event): void {
    const xhr = <XMLHttpRequest>event.target;
    if (xhr.readyState === XHR_DONE) {
      if (xhr.status === HTTP_STATUS_OK) {
        this.ingestIDToken(xhr.response.id_token);
      } else if (xhr.status === HTTP_STATUS_UNAUTHORIZED) {
        this.logout(this.currentPath());
      } else {
        // TODO 2017/12/15 (sr): is there anything we could do that's better than
        // this?
        console.log(`Session refresh failed: ${xhr.statusText}`);
      }
    }
  }

  ingestIDToken(idToken: string): void {
    const id = Jwt.parseIDToken(idToken);
    if (id === null) {
      return;
    }
    this.setSession(
      id.sub,
      id.name,
      // What dex considers email, we consider username (for local users at least)
      // We might have to revisit this given more information about actual LDAP/SAML
      // usage.
      id.email,
      idToken,
      id.groups);
  }

  // canActivate determines if any of the routes (except signin) can be activated
  canActivate(_route: ActivatedRouteSnapshot, state: RouterStateSnapshot): boolean {
    if (!this.hasSession()) {
      this.logout(state.url);
      return false;
    }
    return true;
  }

  // tryInitializeSession tries to lookup session information from localStorage
  tryInitializeSession(): void {
    // Note: it ignores failures because it should also work when injected into
    // SigninComponent.
    if (!this.hasSession()) {
      return;
    }
    this.user = <ChefSessionUser>JSON.parse(localStorage.getItem(sessionKey));
    this.user.telemetry_enabled = this.fetchTelemetryPreference();
  }

  // setSession sets ChefSession's session data in localStorage for having it
  // available for setSessionOrRedirectToLogin() (part of ChefSession's
  // constructor)
  setSession(uuid, fullname, username, id_token: string, groups: Array<string>): void {
    this.user = <ChefSessionUser>{
      uuid,
      fullname,
      username,
      groups,
      id_token
    };
    localStorage.setItem(sessionKey, JSON.stringify(this.user));
  }

  // deleteSession removes the session information from localStorage
  deleteSession(): void {
   localStorage.removeItem(sessionKey);
  }

  // hasSession checks if there's session info in localStorage
  hasSession(): boolean {
    return !isNull(localStorage.getItem(sessionKey));
  }

  logout(url = '/'): void {
    this.deleteSession();
    // note: url will end up url-encoded in this string (magic)
    window.location.href = `/session/new?state=${url}`;
  }

  storeTelemetryPreference(isOptedIn: boolean): void {
    localStorage.setItem(this.userTelemetryStorageKey(), this.booleanToString(isOptedIn));
    if (this.user) {
      this.user.telemetry_enabled = isOptedIn;
    }
  }

  currentPath(): string {
    return window.location.pathname;
  }

  get username(): string {
    return this.user.username;
  }

  get fullname(): string {
    return this.user.fullname;
  }

  get groups(): Array<string> {
    return this.user.groups;
  }

  get id_token(): string {
    return this.user.id_token;
  }

  get telemetry_enabled(): boolean {
    return this.user.telemetry_enabled;
  }

  get uuid(): string {
    return this.user.uuid;
  }

  public fetchTelemetryPreference(): boolean | null {
    let telemetryEnabled: boolean | null;
    const telemStored = localStorage.getItem(this.userTelemetryStorageKey());
    if (isNull(telemStored)) {
      telemetryEnabled = null;
    } else {
      telemetryEnabled = this.stringToBoolean(telemStored);
    }
    return telemetryEnabled;
  }

  public userWelcomeModalSeenKey(): string {
    return !isNil(this.user) ? `${this.uuid}-${this.MODAL_HAS_BEEN_SEEN_KEY}` : null;
  }

  private userTelemetryStorageKey(): string {
    return !isNil(this.user) ? `${this.uuid}-telemetry-enabled` : null;
  }

  private booleanToString(bool: boolean): string {
    return bool ? 'true' : 'false';
  }

  private stringToBoolean(boolString: string): boolean {
    return boolString === 'true';
  }

  setDefaultSession(): void {
    localStorage.setItem(sessionKey,
                         JSON.stringify({
                           'uuid': 'test_subject',
                           'username': 'testchefuser',
                           'fullname': 'Test User',
                           'groups': ['group1', 'group2', 'group3'],
                           'id_token': 'test_id_token'
                         }));
  }
}
