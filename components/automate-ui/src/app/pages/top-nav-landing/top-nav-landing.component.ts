import { Component } from '@angular/core';
import { RoutePerms } from 'app/components/landing/landing.component';

@Component({
  selector: 'app-top-nav-landing',
  template: '<app-landing [routePerms]="routeList"></app-landing>',
  styleUrls: []
})
export class TopNavLandingComponent {

  public routeList: RoutePerms[] = [
  ];
}
