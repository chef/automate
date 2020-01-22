import { Injectable } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';

import { LayoutSidebarService } from './layout-sidebar.service';
import { notificationState } from 'app/entities/notifications/notification.selectors';
import { Notification, Type } from 'app/entities/notifications/notification.model';

import * as fromLayout from './layout.reducer';
import { MenuItemGroup } from './layout.model';
import { sidebarMenuGroups, showPageLoading } from './layout.selectors';
import { ShowPageLoading, UpdateSidebarMenuGroups } from './layout.actions';

import { Project } from 'app/entities/projects/project.model';
import { allProjects } from 'app/entities/projects/project.selectors';

// Important! These must match components/automate-ui/src/styles/_variables.scss
enum Height {
  Navigation = 70,
  Banner = 30,
  ProcessProgressBar = 48,
  PendingEditsBar = 36
}

@Injectable({
  providedIn: 'root'
})
export class LayoutFacadeService {
  // headerHeight = '70px';
  // contentHeight = `calc(100% - ${this.headerHeight})`;
  contentHeight = `calc(100% - ${Height.Navigation}px)`;
  totalElementsHeights = 0;
  menuGroups$: Observable<MenuItemGroup[]>;
  showPageLoading$: Observable<boolean>;
  showLicenseNotification = false;
  showHeader = true;
  showSidebar = true;
  private isDestroyed = new Subject<boolean>();

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
          if (this.showLicenseNotification) {
            this.totalElementsHeights += Height.Banner;
            this.updateContentHeight();
          } else {
            this.totalElementsHeights -= Height.Banner;
            this.updateContentHeight();
          }
      });

    this.store.select(allProjects).pipe(
        takeUntil(this.isDestroyed)
      ).subscribe((projectList: Project[]) => {
        if(projectList.some(p => p.status === 'EDITS_PENDING')) {
          this.totalElementsHeights += Height.PendingEditsBar;
          this.updateContentHeight();
        }
      });
      this.totalElementsHeights += Height.Navigation;
      this.updateContentHeight();
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  /* 
    Pass true for fullPage
  */
  updateContentHeight(fullPage? : boolean): void {
    const combinedHeights = fullPage ? 0 : this.totalElementsHeights;
    this.contentHeight = `calc(100% - ${combinedHeights}px))`;
  }

  hasGlobalNotifications(): boolean {
    return this.showLicenseNotification;
  }

  ShowPageLoading(showLoading: boolean): void {
    this.store.dispatch(new ShowPageLoading(showLoading));
  }

  showFullPage(): void {
    this.updateContentHeight(true);
    this.showSidebar = false;
    this.showHeader = false;
  }

  hideFullPage(): void {
    this.updateContentHeight();
    this.showSidebar = true;
    this.showHeader = true;
  }

  // private updateContentHeight(height: string): void {
  //   this.headerHeight = height;
  //   this.contentHeight = `calc(100% - ${this.headerHeight})`;
  // }

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
