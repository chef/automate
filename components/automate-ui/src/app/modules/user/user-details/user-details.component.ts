import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { FormBuilder, Validators, FormGroup } from '@angular/forms';
import { Store, select } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity, isNil } from 'lodash/fp';

import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { ChefValidators } from 'app/helpers/auth/validator';
import { routeParams } from 'app/route.selectors';
import { EntityStatus, loading } from 'app/entities/entities';
import {
  DeleteUser,
  GetUser,
  UpdateUser,
  UpdateSelf
 } from 'app/entities/users/user.actions';
import {
  getStatus, userFromRoute, updateStatus, deleteStatus
} from 'app/entities/users/user.selectors';
import { User } from 'app/entities/users/user.model';
import { Regex } from 'app/helpers/auth/regex';

@Component({
  selector: 'app-user-details',
  templateUrl: './user-details.component.html',
  styleUrls: ['./user-details.component.scss']
})
export class UserDetailsComponent implements OnInit, OnDestroy {
  public isAdminView = false;
  public user: User;
  public modalVisible = false;
  public editForm: FormGroup;
  public passwordForm: FormGroup;
  private isDestroyed = new Subject<boolean>();
  private deletingUser = false;
  public updatingUser = false;
  public preUrlUserId = '';

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private route: ActivatedRoute,
    private fb: FormBuilder,
    private layoutFacade: LayoutFacadeService
  ) {
    this.createForms(this.fb);
    }

  ngOnInit(): void {
    this.store.pipe(
      select(routeParams),
      pluck('id'),
      filter(identity),
      takeUntil(this.isDestroyed))
      .subscribe((id: string) => {
        if (this.preUrlUserId === '') {
          this.store.dispatch(new GetUser({ id }));
          this.preUrlUserId = id;
        } else {
          this.store.dispatch(new GetUser({ id: this.preUrlUserId }));
        }
      });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(userFromRoute)
    ]).pipe(
      filter(([status, user]) => status === EntityStatus.loadingSuccess && !isNil(user)),
      takeUntil(this.isDestroyed))
      .subscribe(([_, user]) => {
        this.user = { ...user };
      });

    this.route.data.pipe(
      takeUntil(this.isDestroyed))
      .subscribe((data: { isNonAdmin: boolean }) => {
        this.isAdminView = !data.isNonAdmin;

        if (this.isAdminView) {
          this.layoutFacade.showSettingsSidebar();
        } else {
          this.layoutFacade.showUserProfileSidebar();
        }

        // Update the forms once this info has come in
        this.createForms(this.fb);
        this.resetForms();
      });

    this.store.select(updateStatus).pipe(
      takeUntil(this.isDestroyed),
      filter(status => this.updatingUser && !loading(status)))
      .subscribe(() => {
        this.updatingUser = false;
        // same status is used for updating password or full name, so we just reset both
        this.resetForms();
      });

    this.store.pipe(
      select(deleteStatus),
      takeUntil(this.isDestroyed),
      filter(status => this.deletingUser && !loading(status)))
      .subscribe((status) => {
        this.deletingUser = false;
        this.closeDeleteConfirmationModal();
        if (status === EntityStatus.loadingSuccess) {
          this.router.navigate(['/settings', 'users']);
        }
    });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  private createForms(fb: FormBuilder): void {
    // Must stay in sync with error checks in user-details.component.html
    this.editForm = fb.group({
      fullName: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });
    this.passwordForm = fb.group({
      oldPassword: ['', [ChefValidators.nonAdminLengthValidator(this.isAdminView, 8)]],
      newPassword: ['',
        [Validators.required,
        Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.minLength(8)]],
      confirmPassword: ['',
        [Validators.required, ChefValidators.matchFieldValidator('newPassword')]]
    });
  }

  private resetForms(): void {
    this.editForm.reset();
    this.updatingUser = false;
    this.passwordForm.reset();
  }

  public savePassword(): void {
    if (this.isAdminView) {
      this.savePasswordForUser();
    } else {
      this.savePasswordForSelf();
    }
  }

  private savePasswordForUser(): void {
    const password = this.passwordForm.get('newPassword').value;
    this.store.dispatch(new UpdateUser({ ...this.user, password }));
  }

  private savePasswordForSelf(): void {
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

  public saveFullName(): void {
    const name = this.editForm.get('fullName').value.trim();
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
    this.deletingUser = true;
    this.store.dispatch(new DeleteUser(this.user));
  }

  public setUpdatingUser(status: boolean): void {
    this.editForm.patchValue({fullName: this.user.name});
    this.updatingUser = status;
  }
}
