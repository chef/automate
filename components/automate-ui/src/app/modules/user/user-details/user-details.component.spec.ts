import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ActivatedRoute, Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { Subject } from 'rxjs';
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
import { User } from 'app/entities/users/user.model';
import {
  GetUser,
  GetUserSuccess
} from 'app/entities/users/user.actions';
import { UserDetailsComponent } from './user-details.component';

describe('UserDetailsComponent', () => {
  let component: UserDetailsComponent;
  let fixture: ComponentFixture<UserDetailsComponent>;
  let store: Store<NgrxStateAtom>;
  let router: Router;
  const isNonAdmin = new Subject<{ isNonAdmin: boolean }>();

  const user: User = {
    name: 'Alice Schmidt',
    id: 'alice',
    membership_id: '6e98f609-586d-4816-a6de-e841e659b11d'
  };

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
    users: UserEntityInitialState
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
        { provide: ActivatedRoute, useValue: {data: isNonAdmin} }
      ]
    });
    store = TestBed.get(Store);
    spyOn(store, 'dispatch').and.callThrough();
    router = TestBed.get(Router);
    spyOn(router, 'navigate').and.stub();

    jasmine.addMatchers(customMatchers);
    fixture = TestBed.createComponent(UserDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  describe('with populated state', () => {
    beforeEach(() => {
      // Note: this is the happy end of a "fetch user data" exchange with the
      // backend -- so, for setting the stage for testing, it is exactly what
      // we need
      store.dispatch(new GetUserSuccess(user));
    });

    it('should be created', () => {
      expect(component).toBeTruthy();
    });

    it('defaults to showing details section', () => {
      expect(component.tabValue).toBe('details');
    });

    it('show password section when password tab is selected', () => {
      component.onSelectedTab({ target: { value: 'password' } });
      expect(component.tabValue).toBe('password');
    });

    it('show details section when details tab is selected', () => {
      component.onSelectedTab({ target: { value: 'details' } });
      expect(component.tabValue).toBe('details');
    });

    it('picks up the user from state', () => {
      expect(component.user).toEqual(user);
    });

    it('dispatches an action to get its user data', () => {
      expect(store.dispatch).toHaveBeenCalledWith(
        new GetUser({ id: user.id }));
    });
  });

  // this is what happens when opening the browser on /settings/users/alice instead
  // of navigating there from the user mgmt list
  describe('without populated state', () => {
    it('exists', () => {
      expect(component).toBeTruthy();
    });

   it('dispatches an action to get its user data', () => {
      expect(store.dispatch).toHaveBeenCalledWith(
        new GetUser({ id: user.id }));
    });
  });
});
