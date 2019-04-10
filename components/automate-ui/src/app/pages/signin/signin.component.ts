import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { IDToken, Jwt } from 'app/helpers/jwt/jwt';

@Component({
  selector: 'app-signin',
  templateUrl: './signin.component.html',
  styleUrls: ['./signin.component.scss']
})
export class SigninComponent implements OnInit {
  public error = false;
  public path: string; // from OIDC redirect, but sanitized
  private idToken: string;
  private id: IDToken;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private session: ChefSessionService
  ) { }

  ngOnInit() {
    // Reminder: URL fragment has to be treated as user-provided input, and can
    //           NOT be trusted in any way. /!\
    this.route.fragment.subscribe((fragment: string) => {
      if (fragment === null) {
        this.error = true;
        return;
      }
      let state: string;
      [this.idToken, state] = this.idTokenAndStateFromFragment(fragment);
      if (this.idToken === null) {
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
      this.router.navigateByUrl(this.path);
    });
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
      this.id.groups);
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

  idTokenAndStateFromFragment(fragment: string): [string | null, string | null] {
    // Note: we only get an ID token and state now, so we match from ^ to $
    const matches = fragment.match('^id_token=([^&]+)&state=([^&]*)$');
    return matches ? [matches[1], matches[2]] : [null, null];
  }
}
