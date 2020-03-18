import { Injectable } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';

import { LayoutSidebarService } from './layout-sidebar.service';
import * as fromLayout from './layout.reducer';
import { MenuItemGroup } from './layout.model';
import { sidebar,  showPageLoading } from './layout.selectors';
import { ShowPageLoading } from './layout.actions';

import { GetProjects } from 'app/entities/projects/project.actions';

// Important! These must match components/automate-ui/src/styles/_variables.scss
enum Height {
  Navigation = 70,
  License = 39,
  ProcessProgressBar = 54,
  PendingEditsBar = 52
}

export enum Sidebar {
  Dashboards = 'dashboards',
  Applications = 'applications',
  Infrastructure = 'infrastructure',
  Compliance = 'compliance',
  Settings = 'settings',
  Profile = 'profile'
}

@Injectable({
  providedIn: 'root'
})
export class LayoutFacadeService {
  public layout = {
    license: {
      display: true
    },
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
  public sidebar$: Observable<MenuItemGroup[]>;
  public showPageLoading$: Observable<boolean>;

  constructor(
    private store: Store<fromLayout.LayoutEntityState>,
    private layoutSidebarService: LayoutSidebarService
  ) {
    this.sidebar$ = store.select(sidebar);
    this.showPageLoading$ = store.select(showPageLoading);
    this.updateDisplay();
    this.store.dispatch(new GetProjects());
  }

  getContentStyle(): any {
    return { 'height': this.contentHeight };
  }

  updateDisplay(): void {
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
    return this.layout.license.display;
  }

  ShowPageLoading(showLoading: boolean): void {
    setTimeout(() => this.store.dispatch(new ShowPageLoading(showLoading)));
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

  showSidebar(sidebarName: string) {
    this.layoutSidebarService.updateSidebars(sidebarName);
  }
}
