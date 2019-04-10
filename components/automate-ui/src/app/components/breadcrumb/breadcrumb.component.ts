import { Component, Input } from '@angular/core';

@Component({
  selector: 'chef-breadcrumb',
  templateUrl: './breadcrumb.component.html',
  styleUrls: ['./breadcrumb.component.scss']
})
export class BreadcrumbComponent {
  // The link value is passed to an Angular routerLink (https://angular.io/api/router/RouterLink)
  // so it must use the same type.
  @Input() link: any[] | string;

  constructor() { }
}
