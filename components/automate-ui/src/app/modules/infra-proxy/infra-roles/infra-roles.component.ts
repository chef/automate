import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { takeUntil, filter } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { EntityStatus } from 'app/entities/entities';
import { GetRoles, RoleSearch } from 'app/entities/infra-roles/infra-role.action';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';
import {
  allInfraRoles,
  getAllStatus as getAllRolesForOrgStatus,
  getSearchStatus
} from 'app/entities/infra-roles/infra-role.selectors';

@Component({
  selector: 'app-infra-roles',
  templateUrl: './infra-roles.component.html',
  styleUrls: ['./infra-roles.component.scss']
})

export class InfraRolesComponent implements OnInit, OnDestroy {
  @Input() serverId: string;
  @Input() orgId: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();

  private isDestroyed = new Subject<boolean>();
  public roles: InfraRole[] = [];
  public rolesListLoading = true;
  public authFailure = false;
  public searchItems = false;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    this.store.dispatch(new GetRoles({
      server_id: this.serverId, org_id: this.orgId
    }));

    combineLatest([
      this.store.select(getAllRolesForOrgStatus),
      this.store.select(allInfraRoles)
    ]).pipe(takeUntil(this.isDestroyed))
      .subscribe(([getRolesSt, allInfraRolesState]) => {
        if (getRolesSt === EntityStatus.loadingSuccess && !isNil(allInfraRolesState)) {
          this.roles = allInfraRolesState;
          this.rolesListLoading = false;
        } else if (getRolesSt === EntityStatus.loadingFailure) {
          this.rolesListLoading = false;
          this.authFailure = true;
        }
      });
  }

  searchRoles(currentText: string) {
    this.searchItems = true;
    const payload = {
      roleId: currentText,
      server_id: this.serverId,
      org_id: this.orgId,
      page: 0,
      per_page: this.roles.length
    };
    combineLatest([
      this.store.select(getSearchStatus),
      this.store.select(allInfraRoles)
    ]).pipe(
      filter(([getClientsSt, _ClientsState]) =>
      getClientsSt === EntityStatus.loadingSuccess),
      filter(([_getClientsSt, clientsState]) =>
        !isNil(clientsState)),
      takeUntil(this.isDestroyed))
    .subscribe(([_getClientsSt, clientsState]) => {
      this.roles = clientsState;
    });

    this.store.dispatch(new RoleSearch(payload));

    setTimeout(() => {
      this.searchItems = false;
    }, 2000);


  }

  resetKeyTabRedirection(resetLink: boolean) {
    this.resetKeyRedirection.emit(resetLink);
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
