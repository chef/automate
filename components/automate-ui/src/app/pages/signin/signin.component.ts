import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Params, Router } from '@angular/router';
import { ChefSessionService } from '../../services/chef-session/chef-session.service';
import { IDToken, Jwt } from '../../helpers/jwt/jwt';
import { ReplaySubject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { SigninService } from '../../services/signin/signin.service';
import { LicenseUsageService } from '../../services/license-usage/license-usage.service';


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
    private signinService: SigninService,
    private route: ActivatedRoute,
    private licenseUsageService: LicenseUsageService
  ) {
    this.route.queryParams
    .pipe(takeUntil(this.destroyed$))
    .subscribe(params => {
      this.searchParams = params;
    });
  }

  ngOnInit() {
    this.signinService.callback(this.searchParams)
    .pipe(takeUntil(this.destroyed$))
    .subscribe((res) => {
      this.error = this.setIdAndPath(res.id_token, res.state);
      if (this.error) {
        return;
      }
      this.setSession();
      localStorage.setItem('manual-upgrade-banner', 'true');
      this.router.navigateByUrl(this.path);
      this.licenseUsageService.postAnalyticsUsageDataCall();
    }, (err) => {
      if (err.status === 200 && 'url' in err) {
        window.location.href = err.url;
        return;
      }
      this.error = true;
    });
  }

  ngOnDestroy() {
    this.destroyed$.next(true);
    this.destroyed$.complete();
  }

  setIdAndPath(idToken: string, state: string): boolean {
    let error = false;
    this.idToken = idToken;
    this.path = this.pathFromState(state);
    if (this.idToken === '') {
      error = true;
      return error;
    }
    this.id = Jwt.parseIDToken(this.idToken) || {} as IDToken;
    if (this.id === null) {
      error = true;
    }
    return error;
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
    let path: string = "";
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
}
