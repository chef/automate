import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Store } from '@ngrx/store';
import { Observable ,  Subject } from 'rxjs';
import { takeUntil, map } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import { EntityStatus, loading } from 'app/entities/entities';
import { allUsers, userStatus } from 'app/entities/users/user.selectors';
import {
  CreateUser, DeleteUser, GetUsers, CreateUserPayload
} from 'app/entities/users/user.actions';
import { User } from 'app/entities/users/user.model';

function matchFieldValidator() {
    return (control): { [key: string]: any } => {
      if (!control.root || !control.root.controls) {
        return null;
      }
      const valid = control.value === control.root.controls.password.value;
      return valid ? null : { 'noMatch': { value: control }
    };
  };
}

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

  public userToDelete: User;

  public userStatus$: Observable<EntityStatus>;
  public loading$: Observable<boolean>;
  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  // Inputs to app-user-table
  public sortedUsers$: Observable<User[]>;
  public addButtonText = 'Create User';
  public removeText = 'Delete User';
  public baseUrl = '/auth/users';

  constructor(
    private store: Store<NgrxStateAtom>,
    fb: FormBuilder
  ) {
    this.userStatus$ = store.select(userStatus);
    this.loading$ = store.select(userStatus).pipe(map(loading));
    // Note: the `undefined` is the locale to use for comparison. According to
    // MDN, in Swedish, 'ä' comes after 'z', while in German, it's after 'a':
    // tslint:disable-next-line:max-line-length
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/localeCompare
    this.sortedUsers$ = store.select(allUsers).pipe(
      map((users: User[]) => users.sort(
        (a, b) => {
          // See https://stackoverflow.com/a/38641281 for these options
          const opts = { numeric: true, sensitivity: 'base' };
          // sort by name then by username
          return a.name.localeCompare(b.name, undefined, opts) ||
            a.name.localeCompare(b.name, undefined, { numeric: true}) ||
            a.id.localeCompare(b.id, undefined, opts);
        }
      )));

    this.createUserForm = fb.group({
      // Must stay in sync with error checks in user-form.component.html
      fullname: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      username: ['', [Validators.required, Validators.pattern(USERNAME_PATTERN)]],
      // length validator must be consistent with
      // backend password rules in local-user-service/password/password.go
      password: ['', [Validators.required, Validators.minLength(8)]],
      confirmPassword: ['', [Validators.required, matchFieldValidator()]]
    });
  }

  ngOnInit() {
    this.store.dispatch(new GetUsers());
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

  public handleCreateUser(): void {
    const formValues = this.createUserForm.value;

    const userCreateReq = <CreateUserPayload>{
      name: formValues.fullname,
      id: formValues.username,
      password: formValues.password
    };

    this.store.dispatch(new CreateUser(userCreateReq));

    this.userStatus$.pipe(
      takeUntil(this.isDestroyed))
      .subscribe((status) => {
        if (status !== EntityStatus.loading) {
          this.closeCreateModal();
        }
    });
  }
}
