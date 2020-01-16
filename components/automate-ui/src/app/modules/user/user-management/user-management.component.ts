import { Component, EventEmitter, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Store, select } from '@ngrx/store';
import { Subject, combineLatest } from 'rxjs';
import { takeUntil, filter } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { ChefSorters } from 'app/helpers/auth/sorter';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import { ChefValidators } from 'app/helpers/auth/validator';
import { EntityStatus, pending } from 'app/entities/entities';
import { allUsers, getStatus, createStatus, createError } from 'app/entities/users/user.selectors';
import {
  CreateUser, DeleteUser, GetUsers, CreateUserPayload
} from 'app/entities/users/user.actions';
import { User } from 'app/entities/users/user.model';
import { HttpStatus } from 'app/types/types';

// pattern for valid usernames
const USERNAME_PATTERN = '[0-9A-Za-z_@.+-]+';

@Component({
  selector: 'app-user-management',
  templateUrl: './user-management.component.html',
  styleUrls: ['./user-management.component.scss']
})
export class UserManagementComponent implements OnInit, OnDestroy {
  public createUserForm: FormGroup;
  public passwordForm: FormGroup;

  public deleteModalVisible = false;
  public createModalVisible = false;
  public isLoading = true;
  public userToDelete: User;
  public creatingUser = false;
  // TODO maybe combine these into one smarter emitter?
  public conflictErrorEvent = new EventEmitter<boolean>();
  public passwordErrorEvent = new EventEmitter<boolean>();

  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  // Inputs to app-user-table
  public users: User[] = [];
  public addButtonText = 'Create User';
  public removeText = 'Delete User';
  public baseUrl = '/auth/users';

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    fb: FormBuilder
  ) {
    this.createUserForm = fb.group({
      // Must stay in sync with error checks in user-form.component.html
      displayname: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      username: ['', [Validators.required, Validators.pattern(USERNAME_PATTERN)]],
      // length validator must be consistent with
      // backend password rules in local-user-service/password/password.go
      password: ['', [
        Validators.required,
        Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.minLength(8)]],
      confirmPassword: ['', [Validators.required, ChefValidators.matchFieldValidator('password')]]
    });
  }

  ngOnInit() {
    this.layoutFacade.showSettingsSidebar();
    this.store.dispatch(new GetUsers());
    this.layoutFacade.ShowPageLoading(true);
    combineLatest([
      this.store.select(allUsers),
      this.store.select(getStatus)
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(([_, uStatus]) => uStatus !== EntityStatus.loading)
      ).subscribe(([users, _]: [User[], EntityStatus]) => {
        this.isLoading = false;
        this.layoutFacade.ShowPageLoading(false);
        // Sort naturally first by name, then by id
        this.users = ChefSorters.naturalSort(users, ['name', 'id']);
      });

    this.store.pipe(
      select(createStatus),
      takeUntil(this.isDestroyed),
      filter(state => this.createModalVisible && !pending(state)))
      .subscribe(state => {
        this.creatingUser = false;
        if (state === EntityStatus.loadingSuccess) {
          this.closeCreateModal();
        }
      });

    combineLatest([
      this.store.select(createStatus),
      this.store.select(createError)
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(() => this.createModalVisible),
      filter(([state, error]) => state === EntityStatus.loadingFailure && !isNil(error)))
      .subscribe(([_, error]) => {
        if (error.status === HttpStatus.CONFLICT) {
          this.conflictErrorEvent.emit(true);
        } else if (error.status === HttpStatus.BAD_REQUEST) {
          this.passwordErrorEvent.emit(true);
        } else {
          // Close the modal on any error other than conflict and display in banner.
          this.closeCreateModal();
        }
      });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public openCreateModal(): void {
    this.createUserForm.reset();
    this.createModalVisible = true;
  }

  public closeCreateModal(): void {
    this.createUserForm.reset();
    this.createModalVisible = false;
    // TODO reset errors(conflict, pswd)/modifyUsername on close
    // this.conflictErrorEvent.emit(false);
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

  public createUser(): void {
    this.creatingUser = true;
    const formValues = this.createUserForm.value;

    const userCreateReq = <CreateUserPayload>{
      name: formValues.displayname.trim(),
      id: formValues.username,
      password: formValues.password
    };

    this.store.dispatch(new CreateUser(userCreateReq));
  }

  public showEmptyStateMessage(): boolean {
    return !this.isLoading && this.users.length === 0;
  }

  public showUsersTable(): boolean {
    return !this.isLoading && this.users.length > 0;
  }
}
