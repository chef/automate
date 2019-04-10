import { takeUntil, map } from 'rxjs/operators';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { Observable ,  Subject } from 'rxjs';
import { map as lodashMap } from 'lodash';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from '../../ngrx.reducers';
import * as sidebarActions from '../../services/sidebar/sidebar.actions';
import * as sidebarSelectors from '../../services/sidebar/sidebar.selectors';

@Component({
  selector: 'app-server-org-filter-sidebar',
  templateUrl: './server-org-filter-sidebar.component.html',
  styleUrls: ['./server-org-filter-sidebar.component.scss']
})
export class ServerOrgFilterSidebarComponent implements OnInit, OnDestroy {
  servers: Observable<Array<string>>;
  selectedServers: Observable<Array<string>>;
  organizations: Observable<Array<string>>;
  selectedOrganizations: Observable<Array<string>>;
  serverBarActive = false;
  orgBarActive = false;
  subSidebarActive = false;
  localModeTag = 'Local Mode';
  localModeSource = 'localhost';
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>
  ) { }

  ngOnInit() {
    this.servers = this.store.select(sidebarSelectors.allChefServers).pipe(
      takeUntil(this.isDestroyed),
      map(allChefServers => {
        return allChefServers.map( (chefServer: string) =>
        this.updateLocalhostToLocalMode(chefServer));
      }));

    this.selectedServers = this.store.select(sidebarSelectors.selectedChefServers).pipe(
      takeUntil(this.isDestroyed),
      map(selectedChefServers => {
        return selectedChefServers.map( (chefServer: string) =>
          this.updateLocalhostToLocalMode(chefServer) );
      }));

    this.organizations = this.store.select(sidebarSelectors.allOrgs).pipe(
      takeUntil(this.isDestroyed));

    this.selectedOrganizations = this.store.select(sidebarSelectors.selectedOrgs).pipe(
      takeUntil(this.isDestroyed));
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  closeSidebar() {
    this.subSidebarActive = false;

    if (this.orgBarActive) {
      document.getElementById('org-sub-sidebar-button').focus();
    } else if (this.serverBarActive) {
      document.getElementById('server-sub-sidebar-button').focus();
    }

    setTimeout(() => {
      this.orgBarActive = false;
      this.serverBarActive = false;
    }, 500);
  }

  openSidebar(filterType: string) {
    if (filterType === 'server') {
      this.serverBarActive = true;
      this.orgBarActive = false;
    } else if (filterType === 'org') {
      this.orgBarActive = true;
      this.serverBarActive = false;
    }

    this.subSidebarActive = true;
    document.getElementById('sub-sidebar-close').querySelector('button').focus();
  }

  filter(type: string, filter: Array<string>) {
    if (type === 'servers') {
      const self = this;
      filter = lodashMap(filter, function (server) {
        if (server === self.localModeTag) {
          return self.localModeSource;
        } else {
          return server;
        }
      });
      this.store.dispatch(sidebarActions.setSelectedSidebarChefServers(filter));
    } else {
      this.store.dispatch(sidebarActions.setSelectedSidebarOrgs(filter));
    }
  }

  private updateLocalhostToLocalMode(chefServer: string): string {
    if (chefServer.toUpperCase() === this.localModeSource.toUpperCase()) {
      return this.localModeTag;
    } else {
      return chefServer;
    }
  }
}
