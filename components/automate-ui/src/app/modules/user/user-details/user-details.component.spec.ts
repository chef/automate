import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ActivatedRoute, Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { Subject, Observable } from 'rxjs';
import { MockComponent } from 'ng2-mock-component';

import {
  NgrxStateAtom,
  ngrxReducers,
  defaultInitialState,
  runtimeChecks,
  defaultRouterState,
  defaultRouterRouterState
} from 'app/ngrx.reducers';
import { customMatchers } from 'app/testing/custom-matchers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { UserEntityInitialState } from 'app/entities/users/user.reducer';
import { SelfUser } from 'app/entities/users/userself.model';
import { User } from 'app/entities/users/user.model';
import {
  GetUser,
  GetUserSuccess
} from 'app/entities/users/user.actions';
import {
  GetUserSelf,
  GetUserSelfSuccess
} from 'app/entities/users/userself.actions';
import { UserDetailsComponent } from './user-details.component';
import { EntityStatus } from 'app/entities/entities';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { MockChefSessionService } from 'app/testing/mock-chef-session.service';
import { UserPreferencesService } from 'app/services/user-preferences/user-preferences.service';
import { UserPreferenceTimeformat } from 'app/services/user-preferences/user-preferences.model';
import { SigninUiSetting, UISettings } from 'app/services/user-preferences/signin-ui-settings';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockUserPreferencesService {
  public uiSettings: SigninUiSetting = new UISettings()['local'];
  public timeformat$: Observable<UserPreferenceTimeformat>
   = new Observable<UserPreferenceTimeformat>();
}

class MockTelemetryService {
  getTelemetryCheckboxObservable() {
    return new Subject<boolean>();
  }
}

describe('UserDetailsComponent', () => {
  let component: UserDetailsComponent;
  let fixture: ComponentFixture<UserDetailsComponent>;
  let store: Store<NgrxStateAtom>;
  let router: Router;
  const isNonAdmin = new Subject<{ isNonAdmin: boolean }>();

  describe('logged-in user opening their user page', () => {
    const initialState = {
      ...defaultInitialState,
      router: {
        ...defaultRouterState,
        state: {
          ...defaultRouterRouterState,
          url: '/settings/users/alice',
          params: { id: 'alice' }
        }
      },
      users: UserEntityInitialState,
      userSelf: {
        userSelfId: 'alice',
        getStatus: EntityStatus.loading,
        updateStatus: EntityStatus.loading,
        userSelf: {
          id: 'any',
          name: 'any',
          membership_id: 'any'
        }
      }
    };

    const userSelf: SelfUser = {
      name: 'Alice Schmidt',
      id: 'alice',
      membership_id: '6e98f609-586d-4816-a6de-e841e659b11d'
    };

    beforeEach(() => {
      TestBed.configureTestingModule({
        imports: [
          RouterTestingModule,
          FormsModule,
          ReactiveFormsModule,
          StoreModule.forRoot(ngrxReducers, { initialState, runtimeChecks })
        ],
        declarations: [
          MockComponent({ selector: 'chef-loading-spinner' }),
          MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
          MockComponent({ selector: 'chef-breadcrumbs' }),
          MockComponent({ selector: 'chef-breadcrumb', inputs: ['link']}),
          MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
          MockComponent({ selector: 'app-mini-table', inputs: ['tableData'] }),
          MockComponent({ selector: 'chef-error' }),
          MockComponent({ selector: 'chef-form-field' }),
          MockComponent({ selector: 'chef-icon' }),
          MockComponent({ selector: 'chef-input' }),
          MockComponent({ selector: 'chef-modal', inputs: ['visible'] }),
          MockComponent({ selector: 'chef-page-header' }),
          MockComponent({ selector: 'chef-tab-selector', inputs: ['value']}),
          MockComponent({ selector: 'chef-option' }),
          MockComponent({ selector: 'app-authorized',
                          inputs: ['allOf'],
                          template: '<ng-content></ng-content>' }),
          UserDetailsComponent
        ],
        providers: [
          FeatureFlagsService,
          { provide: ActivatedRoute, useValue: {data: isNonAdmin} },
          { provide: ChefSessionService, useClass: MockChefSessionService },
          { provide: UserPreferencesService, useClass: MockUserPreferencesService },
          { provide: TelemetryService, useClass: MockTelemetryService }
        ]
      });
      store = TestBed.inject(Store);
      spyOn(store, 'dispatch').and.callThrough();
      router = TestBed.inject(Router);
      spyOn(router, 'navigate').and.stub();

      jasmine.addMatchers(customMatchers);
      fixture = TestBed.createComponent(UserDetailsComponent);
      component = fixture.componentInstance;
      fixture.detectChanges();
    });

    describe('navigated to profile page', () => {
      beforeEach(() => {

        // profile page is not admin
        isNonAdmin.next({isNonAdmin: true});
        // Note: this is the happy end of a "fetch user data" exchange with the
        // backend -- so, for setting the stage for testing, it is exactly what
        // we need
        store.dispatch(new GetUserSelfSuccess(userSelf));
      });

      it('should be created', () => {
        expect(component).toBeTruthy();
      });

      it('defaults to showing details section', () => {
        expect(component.tabValue).toBe('details');
      });

      it('shows password section when password tab is selected', () => {
        component.onSelectedTab({ target: { value: 'password' } });
        expect(component.tabValue).toBe('password');
        expect(component.userDetails.passwordForm.controls['previousPassword']).toBeTruthy();
      });

      it('shows details section when details tab is selected', () => {
        component.onSelectedTab({ target: { value: 'details' } });
        expect(component.tabValue).toBe('details');
      });

      it('picks up the user from state', () => {
        expect(component.userDetails.user).toEqual(userSelf);
      });

      it('does not show breadcrumbs', () => {
        expect(component.userDetails.showBreadcrumbs).toEqual(false);
      });

      it('shows previous password', () => {
        expect(component.userDetails.showPreviousPassword).toEqual(true);
      });

      it('dispatches an action to get its user data', () => {
        expect(store.dispatch).toHaveBeenCalledWith(new GetUserSelf());
      });
    });

    describe('navigated from admin users list for the logged-in user', () => {

      beforeEach(() => {
        // admin users page is admin
        isNonAdmin.next({isNonAdmin: undefined});

        // Note: this is the happy end of a "fetch user data" exchange with the
        // backend -- so, for setting the stage for testing, it is exactly what
        // we need
        store.dispatch(new GetUserSelfSuccess(userSelf));
      });

      it('exists', () => {
        expect(component).toBeTruthy();
      });

      it('picks up the user from state', () => {
        expect(component.userDetails.user).toEqual(userSelf);
      });

      it('dispatches an action to get its user data', () => {
          expect(store.dispatch).toHaveBeenCalledWith(new GetUserSelf());
      });

      it('shows breadcrumbs', () => {
        expect(component.userDetails.showBreadcrumbs).toEqual(true);
      });

      it('shows previous password', () => {
        expect(component.userDetails.showPreviousPassword).toEqual(true);
      });

      it('does not show previous password field', () => {
        component.onSelectedTab({ target: { value: 'password' } });
        expect(component.tabValue).toBe('password');
        expect(component.userDetails.passwordForm.controls['previousPassword']).toBeTruthy();
      });
    });
  });

  describe('logged-in user opening another users page', () => {
    const initialState = {
      ...defaultInitialState,
      router: {
        ...defaultRouterState,
        state: {
          ...defaultRouterRouterState,
          url: '/settings/users/alice',
          params: { id: 'alice' }
        }
      },
      users: UserEntityInitialState,
      userSelf: {
        userSelfId: 'bob',
        getStatus: EntityStatus.loading,
        updateStatus: EntityStatus.loading,
        userSelf: {
          id: 'any',
          name: 'any',
          membership_id: 'any'
        }
      }
    };

    const user: User = {
      name: 'Alice Schmidt II',
      id: 'alice',
      membership_id: '6e98f609-586d-4816-a6de-e841e659b11d'
    };

    beforeEach(() => {
      TestBed.configureTestingModule({
        imports: [
          RouterTestingModule,
          FormsModule,
          ReactiveFormsModule,
          StoreModule.forRoot(ngrxReducers, { initialState, runtimeChecks })
        ],
        declarations: [
          MockComponent({ selector: 'chef-loading-spinner' }),
          MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
          MockComponent({ selector: 'chef-breadcrumbs' }),
          MockComponent({ selector: 'chef-breadcrumb', inputs: ['link']}),
          MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
          MockComponent({ selector: 'app-mini-table', inputs: ['tableData'] }),
          MockComponent({ selector: 'chef-error' }),
          MockComponent({ selector: 'chef-form-field' }),
          MockComponent({ selector: 'chef-icon' }),
          MockComponent({ selector: 'chef-input' }),
          MockComponent({ selector: 'chef-modal', inputs: ['visible'] }),
          MockComponent({ selector: 'chef-page-header' }),
          MockComponent({ selector: 'chef-tab-selector', inputs: ['value']}),
          MockComponent({ selector: 'chef-option' }),
          MockComponent({ selector: 'app-authorized',
                          inputs: ['allOf'],
                          template: '<ng-content></ng-content>' }),
          UserDetailsComponent
        ],
        providers: [
          FeatureFlagsService,
          { provide: ActivatedRoute, useValue: {data: isNonAdmin} },
          { provide: ChefSessionService, useClass: MockChefSessionService },
          { provide: TelemetryService, useClass: MockTelemetryService }
        ]
      });
      store = TestBed.inject(Store);
      spyOn(store, 'dispatch').and.callThrough();
      router = TestBed.inject(Router);
      spyOn(router, 'navigate').and.stub();

      jasmine.addMatchers(customMatchers);
      fixture = TestBed.createComponent(UserDetailsComponent);
      component = fixture.componentInstance;
      fixture.detectChanges();
    });

    describe('navigation from admin users', () => {
      beforeEach(() => {
        // admin users page is admin
        isNonAdmin.next({isNonAdmin: undefined});

        // Note: this is the happy end of a "fetch user data" exchange with the
        // backend -- so, for setting the stage for testing, it is exactly what
        // we need
        store.dispatch(new GetUserSuccess(user));
      });

      it('exists', () => {
        expect(component).toBeTruthy();
      });

      it('picks up the user from state', () => {
        expect(component.userDetails.user).toEqual(user);
      });

      it('dispatches an action to get its user data', () => {
          expect(store.dispatch).toHaveBeenCalledWith(
            new GetUser({ id: user.id }));
      });

      it('shows breadcrumbs', () => {
        expect(component.userDetails.showBreadcrumbs).toEqual(true);
      });

      it('does not show previous password', () => {
        expect(component.userDetails.showPreviousPassword).toEqual(false);
      });

      it('does not show previous password field', () => {
        component.onSelectedTab({ target: { value: 'password' } });
        expect(component.tabValue).toBe('password');
        expect(component.userDetails.passwordForm.controls['previousPassword']).toBeUndefined();
      });
    });
  });
});
