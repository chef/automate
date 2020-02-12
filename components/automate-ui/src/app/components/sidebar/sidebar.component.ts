import { Component, Inject } from '@angular/core';
import { Observable } from 'rxjs';
import { find } from 'lodash/fp';

import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { MenuItemGroup } from 'app/entities/layout/layout.model';

@Component({
  selector: 'chef-sidebar',
  templateUrl: './sidebar.component.html',
  styleUrls: ['./sidebar.component.scss']
})
export class SidebarComponent {
  menuGroups$: Observable<MenuItemGroup[]>;

  constructor(
    @Inject(LayoutFacadeService) public layoutFacade: LayoutFacadeService
  ) {
    this.menuGroups$ = layoutFacade.sidebar$;
  }

  public hasAuthroizedMenuItems(menuItemGroup: any): boolean {
    const authorizedItem = find(menuItemGroup.items, (menuItem) => {
      return menuItem.authorized && menuItem.authorized.isAuthorized;
    });
    return authorizedItem !== undefined;
  }

  public isAuthorized($event, menuItem) {
    menuItem.authorized.isAuthorized = $event;
  }
}
