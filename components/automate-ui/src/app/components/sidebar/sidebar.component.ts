import { Component, Inject } from '@angular/core';
import { Observable } from 'rxjs';
import { find } from 'lodash';

import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { MenuItemGroup, MenuItem } from 'app/entities/layout/layout.model';

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

  public isGroupVisible(menuGroup: MenuItemGroup): MenuItem {
    return menuGroup.visible && menuGroup.items && find(menuGroup.items, ['visible', true]);
  }
}
