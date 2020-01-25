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
  public loading$ = new Subject<boolean>();
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
    this.loading$.next(true);
    this.route.data.pipe(
      takeUntil(this.isDestroyed))
      .subscribe((data: { isNonAdmin: boolean }) => {
        // undefined for admin view
        // true for profile view
        if (data.isNonAdmin) {
          this.userDetails = new UserProfileDetails(
            this.store, this.layoutFacade, this.isDestroyed, this.fb);
          this.loading$.next(false);
        } else {
          // Is the user the user logged in?
          // Get the user ID from the URL
          // Get the current logged in user's ID
          // compare
          combineLatest([
            this.store.pipe(
              select(routeParams),
              pluck('id'),
              filter(identity),
              first()),
            this.store.select(userSelfId)
          ]).subscribe(([routeId, currentUserId]: [string, string]) => {
            if (routeId === currentUserId) {
              this.userDetails = new UserAdminSelfDetails(
                this.store, this.layoutFacade, this.isDestroyed, this.fb);
            } else {
              this.userDetails = new UserAdminDetails(routeId,
                this.store, this.layoutFacade, this.isDestroyed, this.fb);
            }
            this.loading$.next(false);
          });
        }
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

  public savePassword(): void {
    this.userDetails.savePassword();
  }

  public saveDisplayName(): void {
    this.userDetails.saveDisplayName();
  }

  public onSelectedTab(event: { target: { value: UserTabName } }): void {
    this.tabValue = event.target.value;
    // Drop the previous fragment and add the incoming fragment.
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }
}

interface UserDetails {
  loading$: Observable<boolean>;
  saveSuccessful: boolean;
  saveInProgress: boolean;
  user: User;
  displayNameForm: FormGroup;
  passwordForm: FormGroup;
  showBreadcrumbs: boolean;
  showPreviousPassword: boolean;

  savePassword(): void;
  saveDisplayName(): void;
}

// This view is used when a user goes to another users detail page.
class UserAdminDetails implements UserDetails {
  public loading$: Observable<boolean>;
  public user: User;
  public saveSuccessful = false;
  public saveInProgress = false;
  public displayNameForm: FormGroup;
  public passwordForm: FormGroup;
  public showBreadcrumbs = true;
  public showPreviousPassword = false;

  constructor(
    userId: string,
    private store: Store<NgrxStateAtom>,
    layoutFacade: LayoutFacadeService,
    isDestroyed: Subject<boolean>,
    fb: FormBuilder) {
      this.store.dispatch(new GetUser({ id: userId }));

      layoutFacade.showSettingsSidebar();
      this.createForms(fb);
      this.resetForms();

      this.loading$ = this.store.select(getUserStatus).pipe(
        map((status: EntityStatus) =>  status !== EntityStatus.loadingSuccess));

      combineLatest([
        this.loading$,
        this.store.select(userFromRoute)
      ]).pipe(
        filter(([loadingUserData, user]) => !loadingUserData && !isNil(user)),
        map(([_, user]) => user),
        takeUntil(isDestroyed))
        .subscribe((user) => {
          this.user = { ...user };
          this.displayNameForm.patchValue({displayName: this.user.name});
        });

      this.store.select(updateUserStatus).pipe(
        takeUntil(isDestroyed),
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

  public savePassword(): void {
    this.saveSuccessful = false;
    this.saveInProgress = true;
    const password = this.passwordForm.get('newPassword').value;
    this.store.dispatch(new UpdatePasswordUser({ ...this.user, password }));
  }

  public saveDisplayName(): void {
    this.saveSuccessful = false;
    this.saveInProgress = true;
    const name = this.displayNameForm.get('displayName').value.trim();
    this.store.dispatch(
      new UpdateNameUser({ ...this.user, name }));
  }

  private createForms(fb: FormBuilder): void {
    // Must stay in sync with error checks in user-details.component.html
    this.displayNameForm = fb.group({
      displayName: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });
    this.passwordForm = fb.group({
      previousPassword: ['', [ChefValidators.nonAdminLengthValidator(true, 8)]],
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
}

// This view is used when a user goes to their own user detail page
// The differnce between this and when visting another users pages is the previous password must
// be entered and when the display name is updated the name in the menu is update.
class UserAdminSelfDetails implements UserDetails {
  public loading$: Observable<boolean>;
  public user: User;
  public saveSuccessful = false;
  public saveInProgress = false;
  public displayNameForm: FormGroup;
  public passwordForm: FormGroup;
  public showBreadcrumbs = true;
  public showPreviousPassword = true;

  constructor(
    private store: Store<NgrxStateAtom>,
    layoutFacade: LayoutFacadeService,
    isDestroyed: Subject<boolean>,
    fb: FormBuilder) {
      this.store.dispatch(new GetUserSelf());

      layoutFacade.showSettingsSidebar();

      this.createForms(fb);
      this.resetForms();

      this.loading$ = this.store.select(getUserSelfStatus).pipe(
        map((status: EntityStatus) =>  status !== EntityStatus.loadingSuccess));

      combineLatest([
        this.loading$,
        this.store.select(userSelf)
      ]).pipe(
        filter(([loadingUserData, user]) => !loadingUserData && !isNil(user)),
        map(([_, user]) => user),
        takeUntil(isDestroyed))
        .subscribe((user) => {
          this.user = { ...user };
          this.displayNameForm.patchValue({displayName: this.user.name});
        });

      this.store.select(updateUserSelfStatus).pipe(
        takeUntil(isDestroyed),
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

  public savePassword(): void {
    this.saveSuccessful = false;
    this.saveInProgress = true;
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
    this.store.dispatch(new UpdateNameSelf({
      id: this.user.id,
      name,
      membership_id: this.user.membership_id
    }));
  }

  private resetForms(): void {
    this.displayNameForm.reset();
    if (this.user) {
      this.displayNameForm.patchValue({displayName: this.user.name});
    }
    this.passwordForm.reset();
  }

  private createForms(fb: FormBuilder): void {
    // Must stay in sync with error checks in user-details.component.html
    this.displayNameForm = fb.group({
      displayName: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });
    this.passwordForm = fb.group({
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
class UserProfileDetails implements UserDetails {
  public loading$: Observable<boolean>;
  public user: User;
  public saveSuccessful = false;
  public saveInProgress = false;
  public displayNameForm: FormGroup;
  public passwordForm: FormGroup;
  public showBreadcrumbs = false;
  public showPreviousPassword = true;

  constructor(
    private store: Store<NgrxStateAtom>,
    layoutFacade: LayoutFacadeService,
    isDestroyed: Subject<boolean>,
    fb: FormBuilder) {
      this.store.dispatch(new GetUserSelf());

      layoutFacade.showUserProfileSidebar();

      this.createForms(fb);
      this.resetForms();

      this.loading$ = this.store.select(getUserSelfStatus).pipe(
        map((status: EntityStatus) =>  status !== EntityStatus.loadingSuccess));

      combineLatest([
        this.loading$,
        this.store.select(userSelf)
      ]).pipe(
        filter(([loadingUserData, user]) => !loadingUserData && !isNil(user)),
        map(([_, user]) => user),
        takeUntil(isDestroyed))
        .subscribe((user) => {
          this.user = { ...user };
          this.displayNameForm.patchValue({displayName: this.user.name});
        });

      this.store.select(updateUserSelfStatus).pipe(
        takeUntil(isDestroyed),
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

  public savePassword(): void {
    this.saveSuccessful = false;
    this.saveInProgress = true;
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
    this.store.dispatch(new UpdateNameSelf({
      id: this.user.id,
      name,
      membership_id: this.user.membership_id
    }));
  }

  private resetForms(): void {
    this.displayNameForm.reset();
    if (this.user) {
      this.displayNameForm.patchValue({displayName: this.user.name});
    }
    this.passwordForm.reset();
  }

  private createForms(fb: FormBuilder): void {
    // Must stay in sync with error checks in user-details.component.html
    this.displayNameForm = fb.group({
      displayName: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });
    this.passwordForm = fb.group({
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
