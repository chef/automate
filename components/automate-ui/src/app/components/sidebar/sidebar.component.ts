import { Component, Inject } from '@angular/core';
import { Observable } from 'rxjs';

import { ConfigService } from 'app/services/config/config.service';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { MenuItemGroup } from 'app/entities/layout/layout.model';

@Component({
  selector: 'chef-sidebar',
  templateUrl: './sidebar.component.html',
  styleUrls: ['./sidebar.component.scss']
})
export class SidebarComponent {
  menuGroups$: Observable<MenuItemGroup[]>;
  deploymentType: string;

  constructor(
    @Inject(LayoutFacadeService) public layoutFacade: LayoutFacadeService,
    private configService: ConfigService
  ) {
    this.configService.getConfig().subscribe((config) => {
      this.deploymentType = config.deploymentType;
      this.menuGroups$ = layoutFacade.sidebar$;
      this.updateMenuGroupVisibility();
    })
  }

  public isAuthorized($event, menuItem, menuGroup) {
    menuItem.authorized.isAuthorized = $event;
    this.checkDeploymentType(menuItem, menuGroup);
    this.setGroupVisibility(menuGroup);
  }

  public checkDeploymentType(menuItem: any, menuGroup: any) {
    if (menuItem.route === '/settings/sso-config' &&
      this.deploymentType !== 'SAAS') {
      menuGroup.visible$ = false;
      menuItem.visible$ = false;
    }
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
    return menuItemGroup.items.some(menuItem =>
      menuItem.authorized && menuItem.authorized.isAuthorized);
  }
}
