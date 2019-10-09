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
    @Inject(LayoutFacadeService) layoutFacade: LayoutFacadeService
  ) {
    this.menuGroups$ = layoutFacade.menuGroups$;
  }
}
