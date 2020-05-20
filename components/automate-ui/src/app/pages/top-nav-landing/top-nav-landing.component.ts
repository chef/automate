import { Component } from '@angular/core';
import { RoutePerms } from 'app/components/landing/landing.component';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { ProductDeployedService } from 'app/services/product-deployed/product-deployed.service';

@Component({
  selector: 'app-top-nav-landing',
  templateUrl: './top-nav-landing.component.html',
  styleUrls: ['./top-nav-landing.component.scss']
})
export class TopNavLandingComponent {

  // order must match layout in navbar.component.html
  public routeList: RoutePerms[] = [
    {
      allOfCheck: [
        ['/api/v0/eventfeed', 'get'],
        ['/api/v0/eventstrings', 'get'],
        ['/api/v0/event_type_counts', 'get'],
        ['/api/v0/event_task_counts', 'get']
      ], route: '/dashboards/event-feed'
    },
    {
      anyOfCheck: [
        ['/api/v0/applications/services', 'get'],
        ['/api/v0/applications/service-groups', 'get']
      ], route: '/applications/service-groups'
    },
    {
      allOfCheck: [
        ['/api/v0/cfgmgmt/nodes', 'get'],
        ['/api/v0/cfgmgmt/stats/node_counts', 'get']
      ], route: '/infrastructure'
    },
    {
      anyOfCheck: [
        ['/api/v0/compliance/reporting/stats/summary', 'post'],
        ['/api/v0/compliance/reporting/stats/failures', 'post'],
        ['/api/v0/compliance/reporting/stats/trend', 'post'],
        ['/api/v0/compliance/scanner/jobs', 'post'],
        ['/api/v0/compliance/scanner/jobs/search', 'post'],
        ['/api/v0/compliance/profiles/search', 'post']
      ], route: '/compliance'
    },
    {
      anyOfCheck: [
        ['/api/v0/datafeed/destinations', 'post'],
        ['/api/v0/notifications/rules', 'get'],
        ['/api/v0/secrets/search', 'post'],
        ['/api/v0/nodemanagers/search', 'post'],
        ['/api/v0/retention/nodes/status', 'get'],
        ['/apis/iam/v2/users', 'get'],
        ['/apis/iam/v2/teams', 'get'],
        ['/apis/iam/v2/tokens', 'get'],
        ['/apis/iam/v2/policies', 'get'],
        ['/apis/iam/v2/roles', 'get'],
        ['/apis/iam/v2/projects', 'get']
      ], route: '/settings'
    }
  ];

  constructor(public layoutFacade: LayoutFacadeService,
    productDeployed: ProductDeployedService) {
    if ( productDeployed.isProductDeployed('desktop') ) {
      this.routeList.splice(1, 0,
        {
          allOfCheck: [
            ['/api/v0/cfgmgmt/nodes', 'get'],
            ['/api/v0/cfgmgmt/stats/node_counts', 'get']
          ], route: '/desktop'
        }
      );
    }
  }

  SetPageCoverage = () => this.layoutFacade.showFullPagePlusTopBar();
}
