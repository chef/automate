import { Component, EventEmitter, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { Subject, combineLatest } from 'rxjs';
import { takeUntil, filter } from 'rxjs/operators';

import { ChefSorters } from 'app/helpers/auth/sorter';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { EntityStatus } from 'app/entities/entities';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { allUsers, getStatus } from 'app/entities/users/user.selectors';
import { DeleteUser, GetUsers } from 'app/entities/users/user.actions';
import { User } from 'app/entities/users/user.model';

@Component({
  selector: 'app-user-management',
  templateUrl: './user-management.component.html',
  styleUrls: ['./user-management.component.scss']
})
export class UserManagementComponent implements OnInit, OnDestroy {
  public deleteModalVisible = false;
  public isLoading = true;
  public userToDelete: User;
  public openUserModal = new EventEmitter<boolean>();

  private isDestroyed = new Subject<boolean>();

  // Inputs to app-user-table
  public users: User[] = [];
  public addButtonText = 'Create User';
  public removeText = 'Delete User';
  public baseUrl = '/auth/users';

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) {
 }

  ngOnInit() {
    this.layoutFacade.showSidebar('settings');
    this.store.dispatch(new GetUsers());
    this.layoutFacade.ShowPageLoading(true);
    combineLatest([
      this.store.select(allUsers),
      this.store.select(getStatus)
    ]).pipe(
      filter(([_, uStatus]) => uStatus !== EntityStatus.loading),
      takeUntil(this.isDestroyed)
      ).subscribe(([users, _]: [User[], EntityStatus]) => {
        this.isLoading = false;
        this.layoutFacade.ShowPageLoading(false);
        // Sort naturally first by name, then by id
        this.users = ChefSorters.naturalSort(users, ['name', 'id']);
      });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public openCreateModal(): void {
    this.openUserModal.emit(true);
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  public startUserDelete(user: User): void {
    this.userToDelete = user;
    this.deleteModalVisible = true;
  }

  public deleteUser(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteUser(this.userToDelete));
  }

  public showEmptyStateMessage(): boolean {
    return !this.isLoading && this.users.length === 0;
  }

  public showUsersTable(): boolean {
    return !this.isLoading && this.users.length > 0;
  }
}
