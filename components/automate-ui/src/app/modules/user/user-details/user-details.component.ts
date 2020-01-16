import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { FormBuilder, Validators, FormGroup } from '@angular/forms';
import { Store, select } from '@ngrx/store';
import { combineLatest, Subject, Observable } from 'rxjs';
import { filter, pluck, takeUntil, map } from 'rxjs/operators';
import { identity, isNil } from 'lodash/fp';

import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { ChefValidators } from 'app/helpers/auth/validator';
import { routeURL, routeParams } from 'app/route.selectors';
import { EntityStatus, loading } from 'app/entities/entities';
import {
  GetUser,
  UpdateUser,
  UpdateSelf
 } from 'app/entities/users/user.actions';
import {
  getStatus, userFromRoute, updateStatus
} from 'app/entities/users/user.selectors';
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
  public editForm: FormGroup;
  public passwordForm: FormGroup;
  private isDestroyed = new Subject<boolean>();
  public updatingUser = false;
  public tabValue: UserTabName = 'details';
  private url: string;
  public saveSuccessful = false;
  public loadingGetUser$: Observable<boolean>;

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
        this.store.dispatch(new GetUser({ id }));
      });

    this.loadingGetUser$ = this.store.select(getStatus).pipe(
        map((status: EntityStatus) =>  status !== EntityStatus.loadingSuccess));

    combineLatest([
      this.loadingGetUser$,
      this.store.select(userFromRoute)
    ]).pipe(
      filter(([loadingGetUser, user]) => !loadingGetUser && !isNil(user)),
      takeUntil(this.isDestroyed))
      .subscribe(([_, user]) => {
        this.user = { ...user };
        this.editForm.patchValue({displayName: this.user.name});
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
      .subscribe((status) => {
        this.updatingUser = false;
        // same status is used for updating password or full name, so we just reset both
        this.saveSuccessful = (status === EntityStatus.loadingSuccess);
        if (this.saveSuccessful) {
          this.resetForms();
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

  private createForms(fb: FormBuilder): void {
    // Must stay in sync with error checks in user-details.component.html
    this.editForm = fb.group({
      displayName: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });
    this.passwordForm = fb.group({
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
    if (this.user) {
      this.editForm.patchValue({displayName: this.user.name});
    }
    this.passwordForm.reset();
  }

  public savePassword(): void {
    this.updatingUser = true;
    this.saveSuccessful = false;
    const password = this.passwordForm.get('newPassword').value;
    this.store.dispatch(new UpdateUser({ ...this.user, password }));
  }

  public saveDisplayName(): void {
    this.saveSuccessful = false;
    this.updatingUser = true;
    const name = this.editForm.get('displayName').value.trim();
    this.store.dispatch(
      this.isAdminView ?
        new UpdateUser({ ...this.user, name })
        : new UpdateSelf({
          id: this.user.id,
          name: name,
          membership_id: this.user.membership_id
        }));
  }

  public onSelectedTab(event: { target: { value: UserTabName } }): void {
    this.tabValue = event.target.value;
    // Drop the previous fragment and add the incoming fragment.
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }
}
