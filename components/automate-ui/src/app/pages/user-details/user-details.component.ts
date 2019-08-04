import { combineLatest, Observable, Subscription } from 'rxjs';

import { Component, OnDestroy, OnInit } from '@angular/core';
import { Router, ActivatedRoute } from '@angular/router';
import { FormBuilder, Validators, FormGroup } from '@angular/forms';
import { Store } from '@ngrx/store';
import { filter, pluck, map } from 'rxjs/operators';
import { find, identity } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams } from 'app/route.selectors';
import { EntityStatus } from 'app/entities/entities';
import {
  DeleteUser,
  GetUser,
  UpdateUser,
  UpdateSelf
 } from 'app/entities/users/user.actions';
import {
  allUsers, userStatus, userFromRoute, updateStatus
} from 'app/entities/users/user.selectors';
import { User } from 'app/entities/users/user.model';
import { Regex } from 'app/helpers/auth/regex';

// TODO: deduplicate (copied from user-management.component.ts)
function matchFieldValidator() {
  return (control): { [key: string]: any } => {
    if (!control.root || !control.root.controls) {
      return null;
    }
    const valid = control.value === control.root.controls.newPassword.value;
    return valid ? null : {
      'noMatch': { value: control }
    };
  };
}

function nonAdminValidator(isAdminView: boolean, minLength: number) {
  return (control): { [key: string]: any } => {
    // Don't error if we are in admin view.
    if (isAdminView) {
      return null;
    }
    if (!control.value) {
      return { 'required': { value: control } };
    }
    if (control.value.length < minLength) {
      return { 'minlength': { value: control } };
    }
    return null;
  };
}

@Component({
  selector: 'app-user-details',
  templateUrl: './user-details.component.html',
  styleUrls: ['./user-details.component.scss']
})
export class UserDetailsComponent implements OnInit, OnDestroy {
  public isAdminView = false;
  public editMode = false;
  public user: User;
  public modalVisible = false;
  public editForm: FormGroup;
  public passwordForm: FormGroup;
  private subscriptions: Subscription[];
  private done$: Observable<boolean>;

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private route: ActivatedRoute,
    private fb: FormBuilder
  ) {
    this.createForms(this.fb);
    // TODO (tc) This needs to be refactored to resemble our other patterns
    // for specific object pages.
    // combineLatest depends on the user object existing already.
    this.user = <User>{};

    this.done$ = <Observable<boolean>>combineLatest(
      store.select(allUsers),
      store.select(userStatus))
      .pipe(
        map(([state, status]) => {
          const id = this.user.id;
          return status === EntityStatus.loadingSuccess && !find({ id }, state);
        }));

    this.subscriptions = [
      store.select(userFromRoute).pipe(
        filter(identity))
        .subscribe((state) => {
          this.user = <User>state;
        }),
      // Note: if the user browses directly to /settings/users/USERNAME, the state
      // will not contain any user data -- so we need to fetch it.
      store.select(routeParams).pipe(
        pluck('id'),
        filter(identity))
        .subscribe((id: string) => {
          store.dispatch(new GetUser({id}));
        }),

      // if the user is gone, go back to list
      // note: we filter(identity) here -- so this will effectively only
      // trigger when done$ is `true`
      this.done$.pipe(filter(identity)).subscribe(
        () => this.router.navigate(['/settings', 'users'])),

      store.select(updateStatus).subscribe(this.handleUpdateStatus.bind(this))
      ];
    }

  ngOnInit(): void {
    this.route.data.subscribe((data: { isNonAdmin: boolean }) => {
      this.isAdminView = !data.isNonAdmin;
      // Update the forms once this info has come in
      this.createForms(this.fb);
      this.resetForms();
    });
  }

  private handleUpdateStatus(state: EntityStatus): void {
    // same status is used for updating password or full name, so we just reset both
    if (state === EntityStatus.loadingSuccess) {
      this.resetForms();
    }
  }

  private createForms(fb: FormBuilder): void {
    // Must stay in sync with error checks in user-details.component.html
    this.editForm = fb.group({
      fullName: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });
    this.passwordForm = fb.group({
      oldPassword: ['', [nonAdminValidator(this.isAdminView, 8)]],
      newPassword: ['',
        [Validators.required,
        Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.minLength(8)]],
      confirmPassword: ['', [Validators.required, matchFieldValidator()]]
    });
  }

  ngOnDestroy() {
    this.subscriptions.forEach((sub: Subscription) => sub.unsubscribe());
  }

  private resetForms(): void {
    this.editMode = false;
    this.passwordForm.reset();
  }

  public updatePassword(): void {
    if (this.isAdminView) {
      this.updatePasswordForUser();
    } else {
      this.updatePasswordForSelf();
    }
  }

  private updatePasswordForUser(): void {
    const password = this.passwordForm.get('newPassword').value;
    this.store.dispatch(new UpdateUser({ ...this.user, password }));
  }

  private updatePasswordForSelf(): void {
    const password = this.passwordForm.get('newPassword').value;
    const previous_password = this.passwordForm.get('oldPassword').value;
    this.store.dispatch(new UpdateSelf({
      id: this.user.id,
      name: this.user.name,
      membership_id: this.user.membership_id,
      password: password,
      previous_password: previous_password
    }));
  }

  public updateFullName(): void {
    const name = this.editForm.get('fullName').value;
    this.store.dispatch(
      this.isAdminView ?
        new UpdateUser({ ...this.user, name })
        : new UpdateSelf({
          id: this.user.id,
          name: name,
          membership_id: this.user.membership_id
        }));
  }

  public openDeleteConfirmationModal(): void {
    this.modalVisible = true;
  }

  public closeDeleteConfirmationModal(): void {
    this.modalVisible = false;
  }

  public deleteUser(): void {
    this.store.dispatch(new DeleteUser(this.user));
    this.closeDeleteConfirmationModal();
  }

  public setEditMode(status: boolean): void {
    this.editForm.patchValue({fullName: this.user.name});
    this.editMode = status;
  }
}
