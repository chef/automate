import { Injectable } from '@angular/core';
import { HttpHeaders, HttpClient, HttpBackend, HttpErrorResponse } from '@angular/common/http';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot } from '@angular/router';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Observable, ReplaySubject, timer, throwError } from 'rxjs';
import { map, mergeMap, filter, retryWhen, delay, catchError } from 'rxjs/operators';
import { isNull, isNil } from 'lodash';
import { BroadcastChannel } from 'broadcast-channel';

import { environment } from 'environments/environment';
import { Jwt, IDToken } from 'app/helpers/jwt/jwt';
import { SetUserSelfID } from 'app/entities/users/userself.actions';
import { AppConfigService } from 'app/services/app-config/app-config.service';

import { UserPreferencesService } from '../user-preferences/user-preferences.service';
import { UISettings } from '../user-preferences/signin-ui-settings';
// Should never be on in production. Modify environment.ts locally
// if you wish to bypass getting a session from dex.
const USE_DEFAULT_SESSION = environment.use_default_session;

export interface ChefSessionUser {
  fullname: string;
  username: string;
  isLocalUser: boolean;
  groups: Array<string>;
  telemetry_enabled?: boolean;
  uuid: string;
  id_token: string;
  connector?: string;
}

const sessionKey = 'chef-automate-user';
const HTTP_STATUS_UNAUTHORIZED = 401;

// TODO 2017/11/06 sr: ChefSessionService implementing the route guard is a bit
// of a hack. Something created for the purpose of route-guarding alone would
// be better, and a good refactoring opportunity.
@Injectable()
export class ChefSessionService implements CanActivate {
  private user: ChefSessionUser;
  private httpHandler: HttpClient;
  private isRefreshing: boolean;
  private tokenProvider: ReplaySubject<string>;

  isIdleTimeoutEnabled = false; // default it's false
  idleTimeout = 30; // 30mins default

  // Session state keys - We use session storage to save state here because
  // the application can be reinitialized multiple time during a single session.
  //// Flag to store whether or not the modal has been displayed this session.
  //// Automatically set when the modal is shown for the first time.
  MODAL_HAS_BEEN_SEEN_KEY = 'welcome-modal-seen';

  constructor(private store: Store<NgrxStateAtom>,
     handler: HttpBackend,
     private userPrefService: UserPreferencesService,
     private appConfigService: AppConfigService) {
    // In dev mode, set a generic session so we don't
    // have to round-trip to the oidc provider (dex).
    setTimeout(() => this.callIdleTimeout(), 30 * 1000);
    // callIdleTimeout will get excuted after 30 sec.

    this.tokenProvider = new ReplaySubject(1);
    if (USE_DEFAULT_SESSION) {
      this.setDefaultSession();
      this.tryInitializeSession();
    } else {
      this.tryInitializeSession();
      this.httpHandler = new HttpClient(handler);
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

      timer(0, minute).pipe(
        filter(() => !this.isRefreshing),
        mergeMap(() => {
          this.isRefreshing = true;
          return this.refresh();
        }),
        retryWhen(error => {
          this.isRefreshing = false;
          return error.pipe(delay(500));  // retry in 500ms if errored
        })
      ).subscribe(
        token => {
          this.ingestIDToken(token);
          this.isRefreshing = false;
        },
        error => {
          this.isRefreshing = false;
          console.log(`Retried after 500ms on error: ${error}`);
          return throwError(`Retried after 500ms on error: ${error}`);
        }
      );
    }
  }

  private refresh(): Observable<string> {
    if (!this.id_token) {
      this.isRefreshing = false;
      console.log('id_token is yet to be retrieved');
      return throwError('id_token is yet to be retrieved');
    }
    const httpOptions = {
      headers: new HttpHeaders({
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${this.id_token}`
      })
    };
    return this.httpHandler.get('/session/refresh', httpOptions).pipe(
      map(obj => {
        return obj['id_token'];
      }),
      catchError(error => {
        if (error instanceof HttpErrorResponse) {
          if (error.status === HTTP_STATUS_UNAUTHORIZED) {
            this.logout();
            return throwError(`Unauthorized: ${error.status}`);
          } else {
            console.log(`Session refresh failed: ${error.statusText}`);
            return throwError(`Session refresh failed: ${error.statusText}`);
          }
        } else {
          console.log(`Error calling /refresh ${error}`);
          return throwError(`Error calling /refresh ${error}`);
        }
      })
    );
  }

  callIdleTimeout(): void {
    this.isIdleTimeoutEnabled = this.appConfigService.isIdleTimeoutEnabled;
    this.idleTimeout = this.appConfigService.idleTimeout;

    console.log(this.appConfigService.isIdleTimeoutEnabled, 'this.appConfigService.isIdleTimeoutEnabled');

    if (this.isIdleTimeoutEnabled && this.idleTimeout > 0) {
      this.idleLogout(this.idleTimeout); // Time in minutes
    }
  }

  ingestIDToken(idToken: string): void {
    if (!idToken) {
      return;
    }
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
      id.groups,
      this.isLocalUserFromId(id));
  }

  // canActivate determines if any of the routes (except signin) can be activated
  canActivate(_route: ActivatedRouteSnapshot, state: RouterStateSnapshot): boolean {
    if (!this.hasSession()) {
      this.logout(state.url, true);
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
    this.store.dispatch(new SetUserSelfID({ id: this.user.username }));
    this.initializeUserPreference(this.user);
  }

  // setSession sets ChefSession's session data in localStorage for having it
  // available for setSessionOrRedirectToLogin() (part of ChefSession's
  // constructor)
  setSession(uuid, fullname, username, id_token: string, groups: Array<string>,
    isLocalUser: boolean): void {
    if (!this.user || username !== this.user.username) {
      this.store.dispatch(new SetUserSelfID({ id: username }));
    }
    this.user = {
      uuid,
      fullname,
      username,
      groups,
      id_token,
      isLocalUser
    };
    this.user.telemetry_enabled = this.fetchTelemetryPreference();
    this.initializeUserPreference(this.user);
    this.tokenProvider.next(id_token);
    localStorage.setItem(sessionKey, JSON.stringify(this.user));
  }

  // blacklistIdToken call /logout endpoint in session-service
  blacklistIdToken(idToken: string): void {
    const httpOptions = {
      headers: new HttpHeaders({
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${idToken}`
      })
    };
    this.httpHandler.get('/session/logout', httpOptions).subscribe(
      (res) => {
        console.log('response', res);
        return;
      },
      (e) => {
        console.log('error', e);
        return;
      });
  }

  // deleteSession removes the session information from localStorage
  deleteSession(): void {
    localStorage.removeItem(sessionKey);
  }

  // hasSession checks if there's session info in localStorage
  hasSession(): boolean {
    return !isNull(localStorage.getItem(sessionKey));
  }

  // url: UI route to go back to when the (next) signin process has succeeded
  // noHint: for the sign in, don't try to skip the method selection
  logout(url?: string, noHint?: boolean, ui_signout?: boolean): void {
      if (ui_signout) {
        this.blacklistIdToken(this.id_token);
      }
      this.deleteSession();
      url = url || this.currentPath();
      // note: url will end up url-encoded in this string (magic)
      let signinURL: string;
      if (!noHint && this.user && this.user.id_token) {
        signinURL = `/session/new?state=${url}&id_token_hint=${this.user.id_token}`;
      } else {
        signinURL = `/session/new?state=${url}`;
      }
      window.location.href = signinURL;
  }

  storeTelemetryPreference(isOptedIn: boolean): void {
    localStorage.setItem(this.userTelemetryStorageKey(), this.booleanToString(isOptedIn));
    if (this.user) {
      this.user.telemetry_enabled = isOptedIn;
    }
  }

  idleLogout(idleTimeout: number): void {
    let idleTime = 0;
    const broadcastChannel = new BroadcastChannel('tabsCheckForIdleTimeout');

    // Increment the idle time counter after every minute.
    setInterval(timerIncrement.bind(this), 60 * 1000);
    window.onload = resetTimer;
    window.onmousemove = resetTimer;
    window.onmousedown = resetTimer;  // catches touchscreen presses as well
    window.ontouchstart = resetTimer; // catches touchscreen swipes as well
    window.onclick = resetTimer;      // catches touchpad clicks as well
    window.onkeydown = resetTimer;
    window.addEventListener('scroll', resetTimer, true);
    broadcastChannel.onmessage = resetTimer;
    // catches different tabs/window of same browser activity

    function resetTimer() {
      // this is for testing multi tabs
      if (idleTime > 0) {
        console.log('called broadcast message on different tab');
        broadcastChannel.postMessage('resetTimer');
      }
      idleTime = 0;
    }

    function timerIncrement() {
      idleTime = idleTime + 1;
      if (idleTime === idleTimeout + 1) {
          this.logout('/', true);
      }
    }
  }

  // TODO(sr) 2019/08/26: I don't think we should use these global variables.
  // Instead, we should take the information about the currently-viewed page
  // from the ngrx store.
  currentPath(): string {
    return window.location.pathname;
  }

  get isLocalUser(): boolean {
    return this.user.isLocalUser;
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
    if (this.user) {
      return this.user.id_token;
    }
    return null;
  }

  get connector(): string {
    return this.user.connector;
  }

  get token_provider(): ReplaySubject<string> {
    return this.tokenProvider;
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
    const telemetryStorage = localStorage.getItem(`${this.uuid}-telemetry-enabled`);
    if(isNull(telemetryStorage)) {
      localStorage.setItem(`${this.uuid}-telemetry-enabled`, this.booleanToString(true));
    }
    return !isNil(this.user) ? `${this.uuid}-telemetry-enabled` : null;
  }

  private booleanToString(bool: boolean): string {
    return bool ? 'true' : 'false';
  }

  private stringToBoolean(boolString: string): boolean {
    return boolString === 'true';
  }

  setDefaultSession(): void {
    this.setSession('test_subject', 'Test User', 'testchefuser',
      'test_id_token', ['group1', 'group2', 'group3'], true);
  }

  isLocalUserFromId(id: IDToken): boolean {
    return id.federated_claims &&
      id.federated_claims.connector_id === 'local';
  }

  initializeUserPreference(user) {
    const id: IDToken = Jwt.parseIDToken(user.id_token);
    if (id && id.federated_claims) {
      user.connector = id.federated_claims.connector_id;
      this.userPrefService.apiEndpoint = '/' + user.username + '/' + user.connector;
      if (!this.userPrefService.uiSettings) {
        const uiSettings = new UISettings();
        this.userPrefService.uiSettings = uiSettings[this.user.connector];
      }
    }
  }
}
