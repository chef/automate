import { Injectable, OnDestroy, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable, Subject } from 'rxjs';

import { LayoutSidebarService } from './layout-sidebar.service';
import * as fromLayout from './layout.reducer';
import { MenuItemGroup } from './layout.model';
import { sidebarMenuGroups, showPageLoading } from './layout.selectors';
import { ShowPageLoading, UpdateSidebarMenuGroups } from './layout.actions';

import { GetProjects } from 'app/entities/projects/project.actions';

// Important! These must match components/automate-ui/src/styles/_variables.scss
enum Height {
  Navigation = 70,
  License = 39,
  ProcessProgressBar = 54,
  PendingEditsBar = 52
}

@Injectable({
  providedIn: 'root'
})
export class LayoutFacadeService implements OnInit, OnDestroy {
  public layout = {
    header: {
      display: true,
      license: false,
      navigation: true
    },
    sidebar: {
      display: true,
      navigation: true
    },
    userNotifications: {
      display: true,
      pendingEdits: false,
      updatesProcessing: false
    }
  };
  public contentHeight = `calc(100% - ${Height.Navigation}px)`;
  public menuGroups$: Observable<MenuItemGroup[]>;
  public showPageLoading$: Observable<boolean>;
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<fromLayout.LayoutEntityState>,
    private layoutSidebarService: LayoutSidebarService
  ) {
    this.menuGroups$ = store.select(sidebarMenuGroups);
    this.showPageLoading$ = store.select(showPageLoading);
    this.updateDisplay();
  }

  ngOnInit(): void {
    this.store.dispatch(new GetProjects());
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  getContentStyle() {
    return { 'height': this.contentHeight };
  }

  updateDisplay() {
    let combinedHeights = 0;
    if (this.layout.header.navigation) {
      combinedHeights += Height.Navigation;
    }
    if (this.layout.header.license) {
      combinedHeights += Height.License;
    }
    // order matters for these two: pending is suppressed if processing
    if (this.layout.userNotifications.updatesProcessing) {
      combinedHeights += Height.ProcessProgressBar;
    } else if (this.layout.userNotifications.pendingEdits) {
      combinedHeights += Height.PendingEditsBar;
    }
   this.contentHeight = `calc(100vh - ${combinedHeights}px)`;
  }

  hasGlobalNotifications(): boolean {
    return this.layout.sidebar.navigation = true;
  }

  ShowPageLoading(showLoading: boolean): void {
    this.store.dispatch(new ShowPageLoading(showLoading));
  }

  showFullPage(): void {
    this.contentHeight = '100vh';
    this.layout.header.display = false;
    this.layout.sidebar.display = false;
    this.layout.userNotifications.display = false;
  }

  hideFullPage(): void {
    this.updateDisplay();
    this.layout.header.display = true;
    this.layout.sidebar.display = true;
    this.layout.userNotifications.display = true;
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
