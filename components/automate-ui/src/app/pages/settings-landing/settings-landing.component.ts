import { Component } from '@angular/core';
import { RoutePerms } from 'app/components/landing/landing.component';

@Component({
  selector: 'app-settings-landing',
  template: '<app-landing [routePerms]="routeList"></app-landing>',
  styleUrls: []
})
export class SettingsLandingComponent {

  // order determined by LayoutSidebarService and vetted by SidebarComponent unit tests
  public routeList: RoutePerms[] = [
    { anyOfCheck: [['/api/v0/notifications/rules', 'get']], route: '/settings/notifications' },
    { anyOfCheck: [['/api/v0/datafeed/destinations', 'post']], route: '/settings/data-feeds' },
    { anyOfCheck: [['/api/v0/retention/nodes/status', 'get']], route: '/settings/data-lifecycle' },
    { anyOfCheck: [['/api/v0/nodemanagers/search', 'post']], route: '/settings/node-integrations' },
    { anyOfCheck: [['/api/v0/secrets/search', 'post']], route: '/settings/node-credentials' },
    { allOfCheck: [['/apis/iam/v2/users', 'get']], route: '/settings/users' },
    { allOfCheck: [['/apis/iam/v2/teams', 'get']], route: '/settings/teams' },
    { allOfCheck: [['/apis/iam/v2/tokens', 'get']], route: '/settings/tokens' },
    { allOfCheck: [['/apis/iam/v2/policies', 'get']], route: '/settings/policies' },
    { allOfCheck: [['/apis/iam/v2/roles', 'get']], route: '/settings/roles' },
    { allOfCheck: [['/apis/iam/v2/projects', 'get']], route: '/settings/projects' },
    { allOfCheck: [['/api/v0/sso/config', 'get']], route: '/settings/sso' }
  ];

}
