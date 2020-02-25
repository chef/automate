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

  public isAuthorized($event, menuItem, menuGroup) {
    menuItem.authorized.isAuthorized = $event;
    this.setGroupVisibility(menuGroup);
  }

  public hasMenuItemsNotRequiringAuthorization(menuItemGroup: any): boolean {
    return menuItemGroup.items.some(menuItem =>
      menuItem.authorized === undefined);
  }

  private updateMenuGroupVisibility(): void {
    this.menuGroups$.subscribe((menuGroups: MenuItemGroup[]) => {
      if (menuGroups) {
        menuGroups.forEach(menuItemGroup => this.setGroupVisibility(menuItemGroup));
      }
    });
  }

  private setGroupVisibility(menuItemGroup: MenuItemGroup): void {
    menuItemGroup.hasVisibleMenuItems =
      this.hasAuthorizedMenuItems(menuItemGroup) ||
      this.hasMenuItemsNotRequiringAuthorization(menuItemGroup);
  }

  private hasAuthorizedMenuItems(menuItemGroup: any): boolean {
    return  menuItemGroup.items.some(menuItem =>
          menuItem.authorized && menuItem.authorized.isAuthorized);
  }
}
