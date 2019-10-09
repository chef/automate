import { Injectable } from '@angular/core';
import { Action, Store } from '@ngrx/store';
import { Observable } from 'rxjs';

import { LayoutSidebarService } from './layout-sidebar.service';

import * as fromLayout from './layout.reducer';
import { MenuItemGroup } from './layout.model';
import { sidebarMenuGroups } from './layout.selectors';
import {
    GetSidebarMenuGroups,
    UpdateSidebarMenuGroups
} from './layout.actions';

@Injectable({
    providedIn: 'root'
})
export class LayoutFacadeService {
    menuGroups$: Observable<MenuItemGroup[]>;

    constructor(
        private store: Store<fromLayout.LayoutEntityState>,
        private layoutSidebarService: LayoutSidebarService
    ) {
        this.menuGroups$ = store.select(sidebarMenuGroups);
    }

    dispatch(action: Action) {
        this.store.dispatch(action);
    }

    getMenuGroups() {
        this.store.dispatch(new GetSidebarMenuGroups());
    }

    updateMenuGroups(menuGroups) {
        this.store.dispatch(new UpdateSidebarMenuGroups(menuGroups));
    }

    showComplianceSidebar() {
        this.store.dispatch(new UpdateSidebarMenuGroups(
            this.layoutSidebarService.getComplianceSidebar()
            ));
    }
    
}
