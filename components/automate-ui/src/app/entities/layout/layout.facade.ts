import { Injectable } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';
import { filter } from 'lodash/fp';

import { LayoutSidebarService } from './layout-sidebar.service';
import { notificationState } from 'app/entities/notifications/notification.selectors';

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
    headerHeight = '70px';
    contentHeight = `calc(100% - ${this.headerHeight})`;
    menuGroups$: Observable<MenuItemGroup[]>;
    showLicenseNotification = false;
    showHeader = true;
    showSidebar = true;

    constructor(
        private store: Store<fromLayout.LayoutEntityState>,
        private layoutSidebarService: LayoutSidebarService
    ) {
        this.menuGroups$ = store.select(sidebarMenuGroups);
        store.select(notificationState).subscribe((notifications) => {
            this.showLicenseNotification = filter((n) =>
                n.type === 'license', notifications).length > 0;
            if (this.showLicenseNotification) {
                this.updateContentHeight('110px');
            } else {
                this.updateContentHeight('70px');
            }
        });
    }

    showFullPage() {
        this.contentHeight = '100%';
        this.showSidebar = false;
        this.showHeader = false;
    }

    hideFullPage() {
        this.updateContentHeight(this.headerHeight);
        this.showSidebar = true;
        this.showHeader = true;
    }

    updateContentHeight(height: string): void {
        this.headerHeight = height;
        this.contentHeight = `calc(100% - ${this.headerHeight})`;
    }

    getMenuGroups() {
        this.store.dispatch(new GetSidebarMenuGroups());
    }

    updateMenuGroups(menuGroups) {
        this.store.dispatch(new UpdateSidebarMenuGroups(menuGroups));
    }

    showDashboardsSidebar() {
        this.store.dispatch(new UpdateSidebarMenuGroups(
            this.layoutSidebarService.getDashboardsSidebar()
            ));
    }

    showApplicationsSidebar() {
        this.store.dispatch(new UpdateSidebarMenuGroups(
            this.layoutSidebarService.getApplicationsSidebar()
            ));
    }

    showInfastructureSidebar() {
        this.store.dispatch(new UpdateSidebarMenuGroups(
            this.layoutSidebarService.getInfastructureSidebar()
            ));
    }

    showComplianceSidebar() {
        this.store.dispatch(new UpdateSidebarMenuGroups(
            this.layoutSidebarService.getComplianceSidebar()
            ));
    }

    showSettingsSidebar() {
        this.store.dispatch(new UpdateSidebarMenuGroups(
            this.layoutSidebarService.getSettingsSidebar()
            ));
    }

    showUserProfileSidebar() {
        this.store.dispatch(new UpdateSidebarMenuGroups(
            this.layoutSidebarService.getUserProfileSidebar()
            ));
    }
}
