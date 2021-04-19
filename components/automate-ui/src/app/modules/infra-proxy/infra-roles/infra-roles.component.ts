import { Component, Input, OnInit,
  OnDestroy, EventEmitter, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { takeUntil, filter } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { GetRoles, DeleteRole } from 'app/entities/infra-roles/infra-role.action';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';
import {
  getAllStatus,
  roleList,
  deleteStatus
} from 'app/entities/infra-roles/infra-role.selectors';
import { EntityStatus } from 'app/entities/entities';

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
  public currentPage = 1;
  public per_page = 9;
  public total: number;

  public roleToDelete: InfraRole;
  public chefInfraViewsFeatureFlagOn: boolean;
  public deleteModalVisible = false;
  private isDestroyed = new Subject<boolean>();

  public openRoleModal = new EventEmitter<boolean>();
  public recipes: any;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private featureFlagsService: FeatureFlagsService
  ) {
    // feature flag enable and disable the create button
    this.chefInfraViewsFeatureFlagOn =
    this.featureFlagsService.getFeatureStatus('chefInfraTabsViews');
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    this.getRolesData();

    combineLatest([
      this.store.select(getAllStatus),
      this.store.select(roleList)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([getRolesSt, RolesState]) => {
      if (getRolesSt === EntityStatus.loadingSuccess && !isNil(RolesState)) {
        this.roleListState = RolesState;
        this.roles = RolesState?.items;
        this.total = RolesState?.total;
        this.rolesListLoading = false;
        this.searching = false;
      } else if (getRolesSt === EntityStatus.loadingFailure) {
        this.rolesListLoading = false;
        this.authFailure = true;
      }
    });

    this.store.select(deleteStatus).pipe(
      filter(status => status === EntityStatus.loadingSuccess),
      takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.searching = true;
        if (this.roles && this.roles.length === 0 &&
          this.currentPage !== 1) {
            this.currentPage = this.currentPage - 1;
        }
        this.getRolesData();
      });
  }

  searchRoles(currentText: string) {
    this.currentPage = 1;
    this.searching = true;
    this.searchValue = currentText;

    this.getRolesData();
  }

  onPageChange(event: number): void {
    this.currentPage = event;
    this.searching = true;
    this.getRolesData();
  }

  getRolesData() {
    const payload = {
      roleName: this.searchValue,
      server_id: this.serverId,
      org_id: this.orgId,
      page: this.currentPage,
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
