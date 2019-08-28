import { Component } from '@angular/core';
import { RoutePerms } from 'app/components/landing/landing.component';

@Component({
  selector: 'app-settings-landing',
  template: '<app-landing [routePerms]="routeList"></app-landing>',
  styleUrls: []
})
export class SettingsLandingComponent {

  // order determined by settings-sidebar template and is vetted by settings-sidebar unit tests
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
    },
    { allOfCheck: [['/iam/v2beta/policies', 'get', '']], route: '/settings/policies' },
    { allOfCheck: [['/iam/v2beta/roles', 'get', '']], route: '/settings/roles' },
    { allOfCheck: [['/iam/v2beta/projects', 'get', '']], route: '/settings/projects' }
  ];

}
