import { Component } from '@angular/core';
import { RoutePerms } from 'app/components/landing/landing.component';

@Component({
  selector: 'app-compliance-landing',
  template: '<app-landing [routePerms]="routeList"></app-landing>',
  styleUrls: []
})
export class ComplianceLandingComponent {

  // order determined by compliance-reporting-sidebar
  // template and is vetted by compliance-reporting-sidebar unit tests
  public routeList: RoutePerms[] = [
    {
      allOfCheck: [['/compliance/reporting/stats/summary', 'post', ''],
        ['/compliance/reporting/stats/failures', 'post', ''],
        ['/compliance/reporting/stats/trend', 'post', '']],
      route: '/compliance/reports'
    },
    {
      allOfCheck: [['/compliance/scanner/jobs', 'post', ''],
      ['/compliance/scanner/jobs/search', 'post', '']],
      route: '/compliance/scan-jobs'
    },
    {
      allOfCheck: [['/compliance/profiles/search', 'post', '']],
      route: '/compliance/profiles'
    }
  ];

}
