import { Component, Input, OnInit,
  OnDestroy, EventEmitter, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { GetRoles, DeleteRole } from 'app/entities/infra-roles/infra-role.action';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';
import {
  getAllStatus,
  roleList
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

  public roles: InfraRole[] = [];
  public roleListState: { items: InfraRole[], total: number };
  public rolesListLoading = true;
  public authFailure = false;
  public searching = false;
  public searchValue = '';
  public page = 1;
  public per_page = 9;
  public total: number;

  public roleToDelete: InfraRole;
  public deleteModalVisible = false;
  private isDestroyed = new Subject<boolean>();

  public openRoleModal = new EventEmitter<boolean>();
  public recipes: any;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    const payload = {
      roleName: '',
      server_id: this.serverId,
      org_id: this.orgId,
      page: this.page,
      per_page: this.per_page
    };

    this.store.dispatch(new GetRoles(payload));


    combineLatest([
      this.store.select(getAllStatus),
      this.store.select(roleList)
    ]).pipe(
      takeUntil(this.isDestroyed))
    .subscribe(([_getRolesSt, RolesState]) => {
      if (!isNil(RolesState)) {
        this.roleListState = RolesState;
        if (this.roleListState.items.length === 0 && this.roleListState.total !== 0) {
          this.store.dispatch(new GetRoles(payload));
          this.rolesListLoading = true;
        } else {
          this.roles = RolesState?.items;
          this.total = RolesState?.total;
          this.rolesListLoading = false;
          this.searching = false;
        }
      }
    });
  }

  searchRoles(currentText: string) {
    this.page = 1;
    this.searching = true;
    this.searchValue = currentText;

    this.getRolesData();
  }

  onPageChange(event: number): void {
    this.page = event;
    this.searching = true;
    this.getRolesData();
  }

  getRolesData() {
    const payload = {
      roleName: this.searchValue,
      server_id: this.serverId,
      org_id: this.orgId,
      page: this.page,
      per_page: this.per_page
    };

    this.store.dispatch(new GetRoles(payload));
  }

  resetKeyTabRedirection(resetLink: boolean) {
    this.resetKeyRedirection.emit(resetLink);
  }

  public openCreateModal(): void {
    this.openRoleModal.emit();
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public startRoleDelete(role: InfraRole): void {
    this.roleToDelete = role;
    this.deleteModalVisible = true;
  }

  public deleteRole(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteRole({
      server_id: this.serverId, org_id: this.orgId, name: this.roleToDelete.name
    }));
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }
}
