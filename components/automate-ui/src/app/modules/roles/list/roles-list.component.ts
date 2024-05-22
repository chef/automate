import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { MatOptionSelectionChange } from '@angular/material/core';
import { Observable, Subject } from 'rxjs';
import { map, takeUntil } from 'rxjs/operators';

import { LayoutFacadeService, Sidebar } from '../../../entities/layout/layout.facade';
import { ChefSorters } from '../../../helpers/auth/sorter';
import { NgrxStateAtom } from '../../../ngrx.reducers';
import { loading } from '../../../entities/entities';
import { GetRoles, DeleteRole } from '../../../entities/roles/role.actions';
import { allRoles, getAllStatus } from '../../../entities/roles/role.selectors';
import { Role } from '../../../entities/roles/role.model';
import { TelemetryService } from '../../../services/telemetry/telemetry.service';

@Component({
  selector: 'app-roles-list',
  templateUrl: './roles-list.component.html',
  styleUrls: ['./roles-list.component.scss']
})

export class RolesListComponent implements OnInit, OnDestroy {
  public loading$: Observable<boolean>;
  public sortedRoles$: Observable<Role[]>;
  public roleToDelete: Role;
  public deleteModalVisible = false;
  private isDestroyed = new Subject<boolean>();
  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private telemetryService: TelemetryService
  ) {
    store.select(getAllStatus).pipe(
      map(loading),
      takeUntil(this.isDestroyed)
    ).subscribe(isLoading =>
      this.layoutFacade.ShowPageLoading(isLoading));

    this.sortedRoles$ = store.select(allRoles).pipe(
      map((roles: Role[]) => ChefSorters.naturalSort(roles, 'name')));
  }

  ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.store.dispatch(new GetRoles());
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public startRoleDelete($event, role: Role): void {
    if ($event.isUserInput) {
      this.roleToDelete = role;
      this.deleteModalVisible = true;
    }
  }

  public deleteRole(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteRole(this.roleToDelete));
    this.telemetryService.track('Settings_Roles_Delete');
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }
}
