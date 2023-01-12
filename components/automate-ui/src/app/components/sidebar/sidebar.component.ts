import { Component, Inject } from '@angular/core';
import { Observable } from 'rxjs';

import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { MenuItemGroup } from 'app/entities/layout/layout.model';
import { LoadRemoteModuleOptions } from '@angular-architects/module-federation';

type PluginOptions =  LoadRemoteModuleOptions & {
  displayName: string;
  componentName: string;
}

@Component({
  selector: 'chef-sidebar',
  templateUrl: './sidebar.component.html',
  styleUrls: ['./sidebar.component.scss']
})

export class SidebarComponent {

  menuGroups$: Observable<MenuItemGroup[]>;


  plugins: PluginOptions[] = [];
  workflow: PluginOptions[] = [];

  lookup(): Promise<PluginOptions[]> {
    return Promise.resolve([
        {
            type: 'module',
            remoteEntry: 'http://localhost:4201/remoteEntry.js',
            exposedModule: './Download',

            displayName: 'Download',
            componentName: 'DownloadComponent'
        },
        {
            type: 'module',
            remoteEntry: 'http://localhost:4201/remoteEntry.js',
            exposedModule: './Upload',

            displayName: 'Upload',
            componentName: 'UploadComponent'
        },
        {
            type: 'module',
            remoteEntry: 'http://localhost:4202/remoteEntry.js',
            exposedModule: './Analyze',

            displayName: 'Analyze',
            componentName: 'AnalyzeComponent'
        },
        {
            type: 'module',
            remoteEntry: 'http://localhost:4202/remoteEntry.js',
            exposedModule: './Enrich',

            displayName: 'Enrich',
            componentName: 'EnrichComponent'
        }
    ] as PluginOptions[]);
}

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

  async ngOnInit(): Promise<void> {
    this.plugins = await this.lookup();
    console.warn('on it fun run', this.plugins)
  }

  add(plugin: PluginOptions): void {
    console.warn('@@@@@####', plugin)
    this.workflow.push(plugin);
  }

}
