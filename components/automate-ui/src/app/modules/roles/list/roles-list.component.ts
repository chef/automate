import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { MatOptionSelectionChange } from '@angular/material/core/option';
import { Observable, Subject } from 'rxjs';
import { map, takeUntil } from 'rxjs/operators';

import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { ChefSorters } from 'app/helpers/auth/sorter';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { loading } from 'app/entities/entities';
import { GetRoles, DeleteRole } from 'app/entities/roles/role.actions';
import { allRoles, getAllStatus } from 'app/entities/roles/role.selectors';
import { Role } from 'app/entities/roles/role.model';

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
    private layoutFacade: LayoutFacadeService
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

  public startRoleDelete($event: MatOptionSelectionChange, role: Role): void {
    if ($event.isUserInput) {
      this.roleToDelete = role;
      this.deleteModalVisible = true;
    }
  }

  public deleteRole(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteRole(this.roleToDelete));
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }
}
