import { Component, Inject } from '@angular/core';
import { Observable } from 'rxjs';

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

  public hasAuthroizedMenuItems(menuItemGroup: any): void {
    menuItemGroup.hasAuthroizedMenuItems =
      menuItemGroup.items.filter(menuItem =>
        menuItem.authorized && menuItem.authorized.isAuthorized).length > 0;
  }

  public isAuthorized($event, menuItem, menuGroup) {
    menuItem.authorized.isAuthorized = $event;
    this.hasAuthroizedMenuItems(menuGroup);
  }
}
