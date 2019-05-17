import { Component, OnInit } from '@angular/core';
import { RoutePerms } from 'app/components/landing/landing.component';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { iamMajorVersion, iamMinorVersion } from 'app/entities/policies/policy.selectors';
import { Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';

@Component({
  selector: 'app-settings-landing',
  template: '<app-landing [routePerms]="routeList"></app-landing>',
  styleUrls: []
})
export class SettingsLandingComponent implements OnInit {

  private iamMajorVersion: string;
  private iamMinorVersion: string;
  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(store: Store<NgrxStateAtom>) {
    store.select(iamMajorVersion).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(majorVersion => {
        this.iamMajorVersion = majorVersion;
      });
    store.select(iamMinorVersion).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(minorVersion => {
        this.iamMinorVersion = minorVersion;
      });
  }

  // order determined by admin-sidebar template and is vetted by admin-sidebar unit tests
  public routeList: RoutePerms[] = [
    { anyOfCheck: [['/notifications/rules', 'get', '']], route: '/settings/notifications' },
    { anyOfCheck: [['/nodemanagers/search', 'post', '']], route: '/settings/node-integrations' },
    { anyOfCheck: [['/secrets/search', 'post', '']], route: '/settings/node-credentials' },
    { anyOfCheck: [['/retention/nodes/status', 'get', '']], route: '/settings/node-lifecycle' },
    { allOfCheck: [['/auth/users', 'get', '']], route: '/settings/users' },
    { allOfCheck: [['/auth/teams', 'get', '']], route: '/settings/teams' },
    {
      anyOfCheck: [['/auth/tokens', 'get', ''], ['/iam/v2beta/tokens', 'get', '']],
      route: '/settings/tokens'
    }
  ];
  ngOnInit() {
    if (this.iamMajorVersion !== 'v1') {
      this.routeList.push(
        { allOfCheck: [['/iam/v2beta/policies', 'get', '']], route: '/settings/policies' },
        { allOfCheck: [['/iam/v2beta/roles', 'get', '']], route: '/settings/roles' }
      );
      if (this.iamMinorVersion === 'v1') {
        this.routeList.push(
          { allOfCheck: [['/iam/v2beta/projects', 'get', '']], route: '/settings/projects' }
        );
      }
    }
  }

}
