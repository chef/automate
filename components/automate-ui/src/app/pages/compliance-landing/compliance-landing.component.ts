import { Component } from '@angular/core';
import { RoutePerms } from 'app/components/landing/landing.component';

@Component({
  selector: 'app-compliance-landing',
  template: '<app-landing [routePerms]="routeList"></app-landing>',
  styleUrls: []
})
export class ComplianceLandingComponent {

  // order determined by LayoutSidebarService and vetted by SidebarComponent unit tests
  public routeList: RoutePerms[] = [
    {
      allOfCheck: [['/api/v0/compliance/reporting/stats/summary', 'post'],
        ['/api/v0/compliance/reporting/stats/failures', 'post'],
        ['/api/v0/compliance/reporting/stats/trend', 'post']],
      route: '/compliance/reports'
    },
    {
      allOfCheck: [['/api/v0/compliance/scanner/jobs', 'post'],
      ['/compliance/scanner/jobs/search', 'post']],
      route: '/compliance/scan-jobs'
    },
    {
      allOfCheck: [['/api/v0/compliance/profiles/search', 'post']],
      route: '/compliance/compliance-profiles'
    }
  ];

}
