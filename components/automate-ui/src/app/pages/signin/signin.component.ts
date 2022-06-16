import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Params, Router } from '@angular/router';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { IDToken, Jwt } from 'app/helpers/jwt/jwt';
import { ReplaySubject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { CallbackService } from 'app/services/signin/signin.service';


@Component({
  selector: 'app-signin',
  templateUrl: './signin.component.html',
  styleUrls: ['./signin.component.scss']
})
export class SigninComponent implements OnInit, OnDestroy {
  public error = false;
  public path: string; // from OIDC redirect, but sanitized
  private idToken: string;
  private id: IDToken;
  private destroyed$: ReplaySubject<boolean> = new ReplaySubject(1);
  private searchParams: Params;

  constructor(
    private router: Router,
    private session: ChefSessionService,
    private callbackService: CallbackService,
    private route: ActivatedRoute
  ) {
    this.route.queryParams
    .pipe(takeUntil(this.destroyed$))
    .subscribe(params => {
      this.searchParams = params;
    });
  }

  ngOnInit() {
    this.callbackService.callback(this.searchParams)
    .pipe(takeUntil(this.destroyed$))
    .subscribe((res) => {
      const state: string = res.state;
      this.idToken = res.id_token;
      if (state === '' || this.idToken === '') {
        this.error = true;
        return;
      }
      this.id = Jwt.parseIDToken(this.idToken);
      if (this.id === null) {
        this.error = true;
        return;
      }
      this.path = this.pathFromState(state);

      this.error = false;
      this.setSession();
      localStorage.setItem('manual-upgrade-banner', 'true');
      this.router.navigateByUrl(this.path);
    }, () => {
      this.error = true;
    });
  }

  ngOnDestroy() {
    this.destroyed$.next(true);
    this.destroyed$.complete();
  }

  deleteIdTokenFromCookie(token: string): void {
    // Expire id_token cookie once it's set in localStorage
    document.cookie = `id_token=${token}; Path=/; Expires=${new Date().toUTCString()};`;
  }

  setSession(): void {
    this.session.setSession(
      this.id.sub,
      this.id.name,
      // What dex considers email, we consider username (for local users at least)
      // We might have to revisit this given more information about actual LDAP/SAML
      // usage.
      this.id.email,
      this.idToken,
      this.id.groups,
      this.session.isLocalUserFromId(this.id));
  }

  // pathFromState parses the URL path from state, and logs eventually occurring
  // errors. If something goes wrong, it returns '/', since the possible issues
  // here are not "stop-the-world" problems.
  pathFromState(state: string): string {
    let path: string;
    try {
      path = decodeURIComponent(state);
    } catch (e) {
      if (e instanceof URIError) {
        console.error(`Unable to parse URL from state: ${e.message}`);
        path = '/';
      }
    }
    return path;
  }

  idTokenAndStateFromCookieAndFragment(fragment: string): [string | null, string | null] {
    // Note: we only get an ID token and state now, so we match from ^ to $
    const state_match = fragment.match('^state=([^&]*)$');
    const id_token_match = `; ${document.cookie}`.match(';\\s*id_token=([^;]+)');
    const id_token = id_token_match ? id_token_match[1] : null;
    const state = state_match ? state_match[1] : null;
    return [id_token, state];
  }
}
