import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { FormBuilder, Validators, FormGroup } from '@angular/forms';
import { Store, select, Action } from '@ngrx/store';

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
  getStatus as getUserSelfStatus, userSelf, updateStatus as updateUserSelfStatus,
  userSelfId
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
  private isDestroyed = new Subject<boolean>();
  public loading = true;
  public tabValue: UserTabName = 'details';
  private url: string;
  public userDetails: UserDetails;

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private route: ActivatedRoute,
    private fb: FormBuilder,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit(): void {
    this.route.data.pipe(
      takeUntil(this.isDestroyed)
    ).subscribe((data: { isNonAdmin: boolean }) => {
      // undefined for admin view
      // true for profile view
      if (data.isNonAdmin) {
        this.layoutFacade.showSidebar('settings');
        this.userDetails = new UserProfileDetails(
          this.store, this.layoutFacade, this.isDestroyed, this.fb);
        this.loading = false;
      } else {
        this.layoutFacade.showSidebar('profile');
        // Is this user the logged-in user?
        combineLatest([
          // Get the user ID from the URL
          this.store.pipe(
            select(routeParams),
            pluck('id'),
            filter(identity),
            first()),
          // Get the current logged in user's ID
          this.store.select(userSelfId)
        ]).subscribe(([routeId, currentUserId]: [string, string]) => {
          if (routeId === currentUserId) {
            this.userDetails = new UserAdminSelfDetails(
              this.store, this.layoutFacade, this.isDestroyed, this.fb);
          } else {
            this.userDetails = new UserAdminDetails(routeId,
              this.store, this.layoutFacade, this.isDestroyed, this.fb);
          }
          this.loading = false;
        });
      }
    });

    this.store.select(routeURL).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe((url: string) => {
      this.url = url;
      const [, fragment] = url.split('#');
      this.tabValue = (fragment === 'password') ? 'password' : 'details';
    });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public onSelectedTab(event: { target: { value: UserTabName } }): void {
    this.tabValue = event.target.value;
    // Drop the previous fragment and add the incoming fragment.
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }

  public handlePasswordInput(): void {
    this.userDetails.passwordForm.get('confirmPassword').updateValueAndValidity();
  }
}

abstract class UserDetails {
  public loading$: Observable<boolean>;
  public user: User;
  public saveSuccessful = false;
  public saveInProgress = false;
  public displayNameForm: FormGroup;
  public passwordForm: FormGroup;
  abstract showBreadcrumbs: boolean;
  abstract showPreviousPassword: boolean;

  constructor(private store: Store<NgrxStateAtom>) {}

  public savePassword(): void {
    this.saveSuccessful = false;
    this.saveInProgress = true;
    const password = this.passwordForm.get('newPassword').value;
    this.store.dispatch(this.createUpdatePasswordUserAction(password));
  }

  public saveDisplayName(): void {
    this.saveSuccessful = false;
    this.saveInProgress = true;
    const name = this.displayNameForm.get('displayName').value.trim();
    this.store.dispatch(this.createUpdateNameUserAction(name));
  }

  protected abstract createPasswordForm(fb: FormBuilder): FormGroup;
  protected abstract createUpdatePasswordUserAction(password: string): Action;
  protected abstract createUpdateNameUserAction(name: string): Action;

  protected resetForms(): void {
    this.displayNameForm.reset();
    if (this.user) {
      this.displayNameForm.patchValue({displayName: this.user.name});
    }
    this.passwordForm.reset();
  }

  protected createForms(fb: FormBuilder): void {
    // Must stay in sync with error checks in user-details.component.html
    this.displayNameForm = fb.group({
      displayName: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });

    this.passwordForm = this.createPasswordForm(fb);
  }
}

// This view is used when a user goes to another users detail page.
class UserAdminDetails extends UserDetails {
  public showBreadcrumbs = true;
  public showPreviousPassword = false;

  constructor(
    userId: string,
    store: Store<NgrxStateAtom>,
    layoutFacade: LayoutFacadeService,
    isDestroyed: Subject<boolean>,
    fb: FormBuilder) {
      super(store);

      store.dispatch(new GetUser({ id: userId }));

      layoutFacade.showSettingsSidebar();
      this.createForms(fb);
      this.resetForms();

      this.loading$ = store.select(getUserStatus).pipe(
        map((status: EntityStatus) =>  status !== EntityStatus.loadingSuccess));

      combineLatest([
        this.loading$,
        store.select(userFromRoute)
      ]).pipe(
        filter(([loadingUserData, user]) => !loadingUserData && !isNil(user)),
        map(([_, user]) => user),
        takeUntil(isDestroyed)
      ).subscribe((user) => {
          this.user = { ...user };
          this.displayNameForm.patchValue({displayName: this.user.name});
      });

      store.select(updateUserStatus).pipe(
        filter(status => this.saveInProgress && !loading(status)),
        takeUntil(isDestroyed)
      ).subscribe((status) => {
          this.saveInProgress = false;
          // same status is used for updating password or display name, so we just reset both
          this.saveSuccessful = (status === EntityStatus.loadingSuccess);
          if (this.saveSuccessful) {
            this.resetForms();
          }
      });
  }

  protected createUpdatePasswordUserAction(password: string): Action {
    return new UpdatePasswordUser({ ...this.user, password });
  }

  protected createUpdateNameUserAction(name: string): Action {
    return new UpdateNameUser({ ...this.user, name });
  }

  protected createPasswordForm(fb: FormBuilder): FormGroup {
    // Notice there is no previous password control added.
    return fb.group({
      newPassword: ['',
        [Validators.required,
        Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.minLength(8)]],
      confirmPassword: ['',
        [Validators.required, ChefValidators.matchFieldValidator('newPassword')]]
    });
  }
}

// This view is used when a user goes to their own user detail page
// The difference between this and when visiting another user's pages is the previous password must
// be entered and when the display name is updated the name in the menu is update.
class UserAdminSelfDetails extends UserDetails {
  public showBreadcrumbs = true;
  public showPreviousPassword = true;

  constructor(
    store: Store<NgrxStateAtom>,
    layoutFacade: LayoutFacadeService,
    isDestroyed: Subject<boolean>,
    fb: FormBuilder) {
      super(store);

      store.dispatch(new GetUserSelf());

      layoutFacade.showSettingsSidebar();

      this.createForms(fb);
      this.resetForms();

      this.loading$ = store.select(getUserSelfStatus).pipe(
        map((status: EntityStatus) =>  status !== EntityStatus.loadingSuccess));

      combineLatest([
        this.loading$,
        store.select(userSelf)
      ]).pipe(
        filter(([loadingUserData, user]) => !loadingUserData && !isNil(user)),
        map(([_, user]) => user),
        takeUntil(isDestroyed)
      ).subscribe((user) => {
          this.user = { ...user };
          this.displayNameForm.patchValue({displayName: this.user.name});
      });

      store.select(updateUserSelfStatus).pipe(
        filter(status => this.saveInProgress && !loading(status)),
        takeUntil(isDestroyed)
      ).subscribe((status) => {
          this.saveInProgress = false;
          // same status is used for updating password or display name, so we just reset both
          this.saveSuccessful = (status === EntityStatus.loadingSuccess);
          if (this.saveSuccessful) {
            this.resetForms();
          }
      });
  }

  protected createUpdatePasswordUserAction(password: string): Action {
    const previous_password = this.passwordForm.get('previousPassword').value;
    return new UpdatePasswordSelf({
      id: this.user.id,
      name: this.user.name,
      membership_id: this.user.membership_id,
      password: password,
      previous_password: previous_password
    });
  }

  protected createUpdateNameUserAction(name: string): Action {
    return new UpdateNameSelf({
      id: this.user.id,
      name,
      membership_id: this.user.membership_id
    });
  }

  protected createPasswordForm(fb: FormBuilder): FormGroup {
    // Adding a previous password control
    return fb.group({
      previousPassword: ['', [ChefValidators.nonAdminLengthValidator(false, 8)]],
      newPassword: ['',
        [Validators.required,
        Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.minLength(8)]],
      confirmPassword: ['',
        [Validators.required, ChefValidators.matchFieldValidator('newPassword')]]
    });
  }
}

// This view is used when a user goes to their profile page.
class UserProfileDetails extends UserDetails {
  public showBreadcrumbs = false;
  public showPreviousPassword = true;

  constructor(
    store: Store<NgrxStateAtom>,
    layoutFacade: LayoutFacadeService,
    isDestroyed: Subject<boolean>,
    fb: FormBuilder) {
      super(store);

      store.dispatch(new GetUserSelf());

      layoutFacade.showUserProfileSidebar();

      this.createForms(fb);
      this.resetForms();

      this.loading$ = store.select(getUserSelfStatus).pipe(
        map((status: EntityStatus) => status !== EntityStatus.loadingSuccess));

      combineLatest([
        this.loading$,
        store.select(userSelf)
      ]).pipe(
        filter(([loadingUserData, user]) => !loadingUserData && !isNil(user)),
        map(([_, user]) => user),
        takeUntil(isDestroyed)
      ).subscribe((user) => {
          this.user = { ...user };
          this.displayNameForm.patchValue({displayName: this.user.name});
      });

      store.select(updateUserSelfStatus).pipe(
        filter(status => this.saveInProgress && !loading(status)),
        takeUntil(isDestroyed)
      ).subscribe((status) => {
          this.saveInProgress = false;
          // same status is used for updating password or display name, so we just reset both
          this.saveSuccessful = (status === EntityStatus.loadingSuccess);
          if (this.saveSuccessful) {
            this.resetForms();
          }
      });
  }

  protected createUpdatePasswordUserAction(password: string): Action {
    const previous_password = this.passwordForm.get('previousPassword').value;
    return new UpdatePasswordSelf({
      id: this.user.id,
      name: this.user.name,
      membership_id: this.user.membership_id,
      password: password,
      previous_password: previous_password
    });
  }

  protected createUpdateNameUserAction(name: string): Action {
    return new UpdateNameSelf({
      id: this.user.id,
      name,
      membership_id: this.user.membership_id
    });
  }

  protected createPasswordForm(fb: FormBuilder): FormGroup {
    // Adding a previous password control
    return fb.group({
      previousPassword: ['', [ChefValidators.nonAdminLengthValidator(false, 8)]],
      newPassword: ['',
        [Validators.required,
        Validators.pattern(Regex.patterns.NON_BLANK),
        Validators.minLength(8)]],
      confirmPassword: ['',
        [Validators.required, ChefValidators.matchFieldValidator('newPassword')]]
    });
  }
}
