import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { FormBuilder, Validators, FormGroup } from '@angular/forms';
import { Store, select } from '@ngrx/store';

import { combineLatest, Subject, Observable } from 'rxjs';
import { filter, pluck, takeUntil, first, map } from 'rxjs/operators';

import { identity, isNil } from 'lodash/fp';

import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { ChefValidators } from 'app/helpers/auth/validator';
import { routeURL, routeParams } from 'app/route.selectors';
import { EntityStatus, loading } from 'app/entities/entities';
import {
  GetUser,
  UpdatePasswordUser,
  UpdateNameUser
 } from 'app/entities/users/user.actions';
 import {
  UpdatePasswordSelf,
  UpdateNameSelf,
  GetUserSelf
 } from 'app/entities/users/userself.actions';
import {
  getStatus as getUserStatus, userFromRoute, updateStatus as updateUserStatus
} from 'app/entities/users/user.selectors';
import {
  getStatus as getUserSelfStatus, userSelf, updateStatus as updateUserSelfStatus
} from 'app/entities/users/userself.selectors';
import { User } from 'app/entities/users/user.model';
import { Regex } from 'app/helpers/auth/regex';

export type UserTabName = 'password' | 'details';

@Component({
  selector: 'app-user-details',
  templateUrl: './user-details.component.html',
  styleUrls: ['./user-details.component.scss']
})
export class UserDetailsComponent implements OnInit, OnDestroy {
  public isAdminView = false;
  public user: User;
  public displayNameForm: FormGroup;
  public passwordForm: FormGroup;
  private isDestroyed = new Subject<boolean>();
  public tabValue: UserTabName = 'details';
  private url: string;
  public saveSuccessful = false;
  public saveInProgress = false;
  public loading$: Observable<boolean>;

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
    this.route.data.pipe(
      takeUntil(this.isDestroyed))
      .subscribe((data: { isNonAdmin: boolean }) => {
        // isNonAdmin is undefined for admin
        // isNonAdmin is true for profile
        this.isAdminView = !data.isNonAdmin;

        if (this.isAdminView) {
          this.store.pipe(
            select(routeParams),
            pluck('id'),
            filter(identity),
            first())
            .subscribe((id: string) => {
                this.store.dispatch(new GetUser({ id }));
            });
          this.layoutFacade.showSettingsSidebar();

          this.loading$ = this.store.select(getUserStatus).pipe(
            map((status: EntityStatus) =>  status !== EntityStatus.loadingSuccess));

          combineLatest([
            this.loading$,
            this.store.select(userFromRoute)
          ]).pipe(
            filter(([loadingUserData, user]) => !loadingUserData && !isNil(user)),
            map(([_, user]) => user),
            takeUntil(this.isDestroyed))
            .subscribe((user) => {
              this.user = { ...user };
              this.displayNameForm.patchValue({displayName: this.user.name});
            });

          this.store.select(updateUserStatus).pipe(
            takeUntil(this.isDestroyed),
            filter(status => this.saveInProgress && !loading(status)))
            .subscribe((status) => {
              this.saveInProgress = false;
              // same status is used for updating password or display name, so we just reset both
              this.saveSuccessful = (status === EntityStatus.loadingSuccess);
              if (this.saveSuccessful) {
                this.resetForms();
              }
            });
        } else {
          this.store.dispatch(new GetUserSelf());

          this.layoutFacade.showUserProfileSidebar();

          this.loading$ = this.store.select(getUserSelfStatus).pipe(
            map((status: EntityStatus) =>  status !== EntityStatus.loadingSuccess));

          combineLatest([
            this.loading$,
            this.store.select(userSelf)
          ]).pipe(
            filter(([loadingUserData, user]) => !loadingUserData && !isNil(user)),
            map(([_, user]) => user),
            takeUntil(this.isDestroyed))
            .subscribe((user) => {
              this.user = { ...user };
              this.displayNameForm.patchValue({displayName: this.user.name});
            });

          this.store.select(updateUserSelfStatus).pipe(
            takeUntil(this.isDestroyed),
            filter(status => this.saveInProgress && !loading(status)))
            .subscribe((status) => {
              this.saveInProgress = false;
              // same status is used for updating password or display name, so we just reset both
              this.saveSuccessful = (status === EntityStatus.loadingSuccess);
              if (this.saveSuccessful) {
                this.resetForms();
              }
            });
        }

        // Update the forms once this info has come in
        this.createForms(this.fb);
        this.resetForms();
      });

    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
      .subscribe((url: string) => {
        this.url = url;
        const [, fragment] = url.split('#');
        this.tabValue = (fragment === 'password') ? 'password' : 'details';
      });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  private createForms(fb: FormBuilder): void {
    // Must stay in sync with error checks in user-details.component.html
    this.displayNameForm = fb.group({
      displayName: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });
    this.passwordForm = fb.group({
      previousPassword: ['', [ChefValidators.nonAdminLengthValidator(this.isAdminView, 8)]],
      newPassword: ['',
        [Validators.required,
        Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.minLength(8)]],
      confirmPassword: ['',
        [Validators.required, ChefValidators.matchFieldValidator('newPassword')]]
    });
  }

  private resetForms(): void {
    this.displayNameForm.reset();
    if (this.user) {
      this.displayNameForm.patchValue({displayName: this.user.name});
    }
    this.passwordForm.reset();
  }

  public savePassword(): void {
    this.saveSuccessful = false;
    this.saveInProgress = true;
    if (this.isAdminView) {
      this.savePasswordForUser();
    } else {
      this.savePasswordForSelf();
    }
  }

  private savePasswordForUser(): void {
    const password = this.passwordForm.get('newPassword').value;
    this.store.dispatch(new UpdatePasswordUser({ ...this.user, password }));
  }

  private savePasswordForSelf(): void {
    const password = this.passwordForm.get('newPassword').value;
    const previous_password = this.passwordForm.get('previousPassword').value;
    this.store.dispatch(new UpdatePasswordSelf({
      id: this.user.id,
      name: this.user.name,
      membership_id: this.user.membership_id,
      password: password,
      previous_password: previous_password
    }));
  }

  public saveDisplayName(): void {
    this.saveSuccessful = false;
    this.saveInProgress = true;
    const name = this.displayNameForm.get('displayName').value.trim();
    if (this.isAdminView) {
      this.store.dispatch(
          new UpdateNameUser({ ...this.user, name }));
    } else {
      this.store.dispatch(new UpdateNameSelf({
        id: this.user.id,
        name: name,
        membership_id: this.user.membership_id
      }));
    }
  }

  public onSelectedTab(event: { target: { value: UserTabName } }): void {
    this.tabValue = event.target.value;
    // Drop the previous fragment and add the incoming fragment.
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }
}

// class UserAdminDetailsComponent {
//   public Init(): void {

//   }

//   public savePassword(): void {

//   }

//   public saveDisplayName(): void {

//   }
// }
