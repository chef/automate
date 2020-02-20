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
    this.updateMenuGroupVisibility();
  }

  public updateMenuGroupVisibility(): void {
    this.menuGroups$.subscribe((menuGroups: MenuItemGroup[]) => {
      if (menuGroups) {
        menuGroups.forEach(menuItemGroup => this.hasVisibleMenuItems(menuItemGroup));
      }
    });
  }

  public hasVisibleMenuItems(menuItemGroup: any): void {
    menuItemGroup.hasAuthorizedMenuItems =
      this.hasAuthorizedMenuItems(menuItemGroup) ||
      this.hasNoAuthorizedMenuItems(menuItemGroup);
  }

  public hasAuthorizedMenuItems(menuItemGroup: any): boolean {
    return menuItemGroup.items.filter(menuItem =>
        menuItem.authorized && menuItem.authorized.isAuthorized).length > 0;
  }

  public hasNoAuthorizedMenuItems(menuItemGroup: any): boolean {
    return menuItemGroup.items.filter(menuItem =>
        menuItem.authorized === undefined).length > 0;
  }

  public isAuthorized($event, menuItem, menuGroup) {
    menuItem.authorized.isAuthorized = $event;
    this.hasVisibleMenuItems(menuGroup);
  }
}
