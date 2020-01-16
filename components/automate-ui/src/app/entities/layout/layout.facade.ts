import { Injectable } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';

import { LayoutSidebarService } from './layout-sidebar.service';
import { notificationState } from 'app/entities/notifications/notification.selectors';
import { Notification, Type } from 'app/entities/notifications/notification.model';

import * as fromLayout from './layout.reducer';
import { MenuItemGroup } from './layout.model';
import { sidebarMenuGroups, showPageLoading } from './layout.selectors';
import { ShowPageLoading, UpdateSidebarMenuGroups } from './layout.actions';

// Important! These must match components/automate-ui/src/styles/_variables.scss
enum Height {
  Navigation = '70px',
  Banner = '110px'
}

@Injectable({
  providedIn: 'root'
})
export class LayoutFacadeService {
  headerHeight = '70px';
  contentHeight = `calc(100% - ${this.headerHeight})`;
  menuGroups$: Observable<MenuItemGroup[]>;
  showPageLoading$: Observable<boolean>;
  showLicenseNotification = false;
  showHeader = true;
  showSidebar = true;

  constructor(
    private store: Store<fromLayout.LayoutEntityState>,
    private layoutSidebarService: LayoutSidebarService
  ) {
    this.menuGroups$ = store.select(sidebarMenuGroups);
    this.showPageLoading$ = store.select(showPageLoading);

    store.select(notificationState).subscribe(
      (notifications: Notification[]) => {
        this.showLicenseNotification =
          notifications &&  notifications.some(n => n.type === Type.license);
        this.updateContentHeight(
          this.showLicenseNotification ? Height.Banner : Height.Navigation);
      });
  }

  hasGlobalNotifications(): boolean {
    return this.headerHeight === Height.Banner;
  }

  ShowPageLoading(showLoading: boolean): void {
    this.store.dispatch(new ShowPageLoading(showLoading));
  }

  showFullPage(): void {
    this.contentHeight = '100%';
    this.showSidebar = false;
    this.showHeader = false;
  }

  hideFullPage(): void {
    this.updateContentHeight(this.headerHeight);
    this.showSidebar = true;
    this.showHeader = true;
  }

  private updateContentHeight(height: string): void {
    this.headerHeight = height;
    this.contentHeight = `calc(100% - ${this.headerHeight})`;
  }

  showDashboardsSidebar(): void {
    this.store.dispatch(new UpdateSidebarMenuGroups(
      this.layoutSidebarService.getDashboardsSidebar()
    ));
  }

  showApplicationsSidebar(): void {
    this.store.dispatch(new UpdateSidebarMenuGroups(
      this.layoutSidebarService.getApplicationsSidebar()
    ));
  }

  showInfrastructureSidebar(): void {
    this.store.dispatch(new UpdateSidebarMenuGroups(
      this.layoutSidebarService.getInfrastructureSidebar()
    ));
  }

  showComplianceSidebar(): void {
    this.store.dispatch(new UpdateSidebarMenuGroups(
      this.layoutSidebarService.getComplianceSidebar()
    ));
  }

  showSettingsSidebar(): void {
    this.store.dispatch(new UpdateSidebarMenuGroups(
      this.layoutSidebarService.getSettingsSidebar()
    ));
  }

  showUserProfileSidebar(): void {
    this.store.dispatch(new UpdateSidebarMenuGroups(
      this.layoutSidebarService.getUserProfileSidebar()
    ));
  }
}
