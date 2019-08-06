import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ActivatedRoute, Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { routerReducer } from '@ngrx/router-store';
import { Subject } from 'rxjs';
import { MockComponent } from 'ng2-mock-component';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { customMatchers } from 'app/testing/custom-matchers';
import {
  userEntityReducer,
  UserEntityInitialState
} from 'app/entities/users/user.reducer';
import { User } from 'app/entities/users/user.model';
import {
  GetUser,
  GetUserSuccess,
  DeleteUser,
  DeleteUserSuccess
} from 'app/entities/users/user.actions';
import { UserDetailsComponent } from './user-details.component';

describe('UserDetailsComponent', () => {
  let component: UserDetailsComponent;
  let fixture: ComponentFixture<UserDetailsComponent>;
  let store: Store<NgrxStateAtom>;
  let router: Router;
  let element: HTMLElement;
  const isNonAdmin = new Subject<{ isNonAdmin: boolean }>();

  const user: User = {
    name: 'Alice Schmidt',
    id: 'alice',
    membership_id: '6e98f609-586d-4816-a6de-e841e659b11d'
  };

  const initialState = {
    router: {
      state: {
        url: '/settings/users/alice',
        params: { id: 'alice' },
        queryParams: {},
        fragment: ''
      },
      navigationId: 0 // what's that zero?
    },
    users: UserEntityInitialState
  };

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        FormsModule,
        ReactiveFormsModule,
        StoreModule.forRoot({
          router: routerReducer,
          users: userEntityReducer
        }, { initialState })
      ],
      declarations: [
        MockComponent({ selector: 'app-admin-sidebar' }),
        MockComponent({ selector: 'app-profile-sidebar' }),
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
        MockComponent({ selector: 'chef-tab-selector' }),
        MockComponent({ selector: 'chef-option' }),
        MockComponent({ selector: 'app-delete-object-modal',
          inputs: ['visible', 'objectNoun', 'objectName'],
          outputs: ['close', 'deleteClicked'] }),
        MockComponent({ selector: 'app-authorized',
                        inputs: ['allOf'],
                        template: '<ng-content></ng-content>' }),
        UserDetailsComponent
      ],
      providers: [
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
    element = fixture.debugElement.nativeElement;
    fixture.detectChanges();
  });

  describe('with populated state', () => {
    beforeEach(() => {
      // Note: this is the happy end of a "fetch user data" exchange with the
      // backend -- so, for setting the stage for testing, it is exactly what
      // we need
      store.dispatch(new GetUserSuccess(user));
    });

    it('exists', () => {
      expect(component).toBeTruthy();
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

  describe('when deleting the user', () => {
    beforeEach(() => {
      store.dispatch(new GetUserSuccess(user));
      component.deleteUser();
    });

    it('dispatches an action to delete its user', () => {
      expect(store.dispatch).toHaveBeenCalledWith(new DeleteUser(user));
    });

    describe('when it was successful', () => {
      beforeEach(() => {
        // this happens when the delete is done:
        store.dispatch(new DeleteUserSuccess(user));
      });

      it('navigates back to the user list when the user is gone', () => {
      component.ngOnInit();
        expect(router.navigate).toHaveBeenCalledWith(['/settings', 'users']);
      });
    });

    // Note (tc): Some conditional components are contained
    // within mocked components so we can't test them.
    describe('when the user is an admin', () => {
      beforeEach(() => {
        component.isAdminView = true;
        fixture.detectChanges();
      });

      it('contains the breadcrumbs', () => {
        expect(element).toContainPath('chef-breadcrumbs');
      });

      it('does not contain the password description', () => {
        expect(element).not.toContainPath('password-description');
      });

      it('contains the admin sidebar instead of the profile sidebar', () => {
        expect(element).toContainPath('app-admin-sidebar');
        expect(element).not.toContainPath('app-profile-sidebar');
      });
    });

    describe('when the user is not an admin', () => {
      beforeEach(() => {
        component.isAdminView = false;
        fixture.detectChanges();
      });

      it('does not contain the breadcrumbs', () => {
        expect(element).not.toContainPath('chef-breadcrumbs');
      });

      it('contains the password description', () => {
        expect(element).toContainPath('#password-description');
      });

      it('contains the profile sidebar instead of the admin sidebar', () => {
        expect(element).not.toContainPath('app-admin-sidebar');
        expect(element).toContainPath('app-profile-sidebar');
      });
    });

  });
});
