import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule, Store } from '@ngrx/store';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { userEntityReducer } from 'app/entities/users/user.reducer';
import { GetUsersSuccess } from 'app/entities/users/user.actions';
import { UserManagementComponent } from './user-management.component';

describe('UserManagementComponent', () => {
  let component: UserManagementComponent;
  let fixture: ComponentFixture<UserManagementComponent>;

  beforeEach(async(() => {

    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'app-admin-sidebar' }),
        MockComponent({ selector: 'app-user-form', inputs: ['createUserForm'] }),
        MockComponent({ selector: 'app-user-table',
            inputs: ['addButtonText', 'addDescription', 'baseUrl', 'users$', 'removeText'] }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-modal', inputs: ['visible'] }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'app-delete-object-modal',
                        inputs: ['default', 'visible', 'objectNoun', 'objectName'],
                        outputs: ['close', 'deleteClicked'] }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        UserManagementComponent
      ],
      imports: [
        ReactiveFormsModule,
        StoreModule.forRoot({
          users: userEntityReducer
        })
      ]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(UserManagementComponent);
    component = fixture.componentInstance;

    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  describe('#createUserForm', () => {
    it('should be invalid when no fields are filled out', () => {
      expect(component.createUserForm.valid).toBeFalsy();
    });

     it('should be valid when all fields are filled out and passwords match', () => {
      component.createUserForm.controls['fullname'].setValue('Sam');
      component.createUserForm.controls['username'].setValue('Mary');
      component.createUserForm.controls['password'].setValue('imawitch');
      component.createUserForm.controls['confirmPassword'].setValue('imawitch');
      expect(component.createUserForm.valid).toBeTruthy();
    });
  });

  describe('#matchFieldValidator', () => {
    beforeEach(() => {
      component.createUserForm.controls['password'].setValue('imawitch');
    });

    it('should be invalid when password and confirmPassword do not match', () => {
      component.createUserForm.controls['confirmPassword'].setValue('notawitch');
      expect(component.createUserForm.get('confirmPassword').hasError('noMatch')).toBeTruthy();
    });

    it('should be valid when password and confirmPassword match', () => {
      component.createUserForm.controls['confirmPassword'].setValue('imawitch');
      expect(component.createUserForm.get('confirmPassword').hasError('noMatch')).toBeFalsy();
    });
  });

  describe('sortedUsers$', () => {
    let store: Store<NgrxStateAtom>;
    beforeEach(() => {
      store = TestBed.get(Store);
    });

    it('intermixes capitals and lowercase with lowercase first', () => {
      store.dispatch(new GetUsersSuccess({ users: [
        { membership_id: 'uuid-1', name: 'Alice', id: 'alice2' },
        { membership_id: 'uuid-2', name: 'alice', id: 'alice1' },
        { membership_id: 'uuid-3', name: 'bob', id: 'builder2001' },
        { membership_id: 'uuid-4', name: 'Bob', id: 'builder2000' }
      ]}));
      component.sortedUsers$.subscribe(users => {
        expect(users.length).toBe(4);
        expect(users[0]).toEqual(jasmine.objectContaining({ name: 'alice' }));
        expect(users[1]).toEqual(jasmine.objectContaining({ name: 'Alice' }));
        expect(users[2]).toEqual(jasmine.objectContaining({ name: 'bob' }));
        expect(users[3]).toEqual(jasmine.objectContaining({ name: 'Bob' }));
      });
    });

    it('sorts by whole string before case', () => {
      store.dispatch(new GetUsersSuccess({ users: [
        { membership_id: 'uuid-1', name: 'alice in wonderland', id: 'alice1' },
        { membership_id: 'uuid-20', name: 'alice', id: 'alice2' },
        { membership_id: 'uuid-2', name: 'Alice', id: 'alice3' }
      ]}));
      component.sortedUsers$.subscribe(users => {
        expect(users.length).toBe(3);
        expect(users[0]).toEqual(jasmine.objectContaining({ name: 'alice' }));
        expect(users[1]).toEqual(jasmine.objectContaining({ name: 'Alice' }));
        expect(users[2]).toEqual(jasmine.objectContaining({ name: 'alice in wonderland' }));
      });
    });

    it('sorts by name then by username', () => {
      store.dispatch(new GetUsersSuccess({ users: [
       { membership_id: 'uuid-22', name: 'Bob', id: 'builder2001'  },
       { membership_id: 'uuid-2', name: 'Bob', id: 'builder2000'  },
       { membership_id: 'uuid-1', name: 'Alice in Wonderland', id: 'alice'  },
       { membership_id: 'uuid-20', name: 'alice', id: 'the-other-alice'  }
     ]}));
     component.sortedUsers$.subscribe(users => {
       expect(users.length).toBe(4);
       expect(users[0]).toEqual(jasmine.objectContaining({ name: 'alice' }));
       expect(users[1]).toEqual(jasmine.objectContaining({ name: 'Alice in Wonderland' }));
       expect(users[2]).toEqual(
         jasmine.objectContaining({ name: 'Bob', id: 'builder2000' }));
       expect(users[3]).toEqual(
         jasmine.objectContaining({ name: 'Bob', id: 'builder2001' }));
     });
    });

    it('uses natural ordering in name', () => {
      store.dispatch(new GetUsersSuccess({ users: [
        { membership_id: 'uuid-1', name: 'Alice01', id: 'alice1' },
        { membership_id: 'uuid-2', name: 'Alice300', id: 'alice2' },
        { membership_id: 'uuid-3', name: 'Alice3', id: 'alice3' },
        { membership_id: 'uuid-4', name: 'Alice-2', id: 'alice4' },
        { membership_id: 'uuid-5', name: 'alice', id: 'alice5' }
      ]}));
      component.sortedUsers$.subscribe(users => {
        expect(users.length).toBe(5);
        expect(users[0]).toEqual(jasmine.objectContaining({ name: 'alice' }));
        expect(users[1]).toEqual(jasmine.objectContaining({ name: 'Alice-2' }));
        expect(users[2]).toEqual(jasmine.objectContaining({ name: 'Alice01' }));
        expect(users[3]).toEqual(jasmine.objectContaining({ name: 'Alice3' }));
        expect(users[4]).toEqual(jasmine.objectContaining({ name: 'Alice300' }));
      });
    });

    it('uses natural ordering in username', () => {
      store.dispatch(new GetUsersSuccess({ users: [
        { membership_id: 'uuid-1', name: 'Alice', id: 'Alice01' },
        { membership_id: 'uuid-2', name: 'Alice', id: 'Alice300' },
        { membership_id: 'uuid-3', name: 'Alice', id: 'Alice3' },
        { membership_id: 'uuid-4', name: 'Alice', id: 'Alice-2' },
        { membership_id: 'uuid-5', name: 'Alice', id: 'alice' }
      ]}));
      component.sortedUsers$.subscribe(users => {
        expect(users.length).toBe(5);
        expect(users[0]).toEqual(jasmine.objectContaining({ id: 'alice' }));
        expect(users[1]).toEqual(jasmine.objectContaining({ id: 'Alice-2' }));
        expect(users[2]).toEqual(jasmine.objectContaining({ id: 'Alice01' }));
        expect(users[3]).toEqual(jasmine.objectContaining({ id: 'Alice3' }));
        expect(users[4]).toEqual(jasmine.objectContaining({ id: 'Alice300' }));
      });
    });

  });
});
