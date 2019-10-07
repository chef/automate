import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule } from '@ngrx/store';

import { runtimeChecks } from 'app/ngrx.reducers';
import { userEntityReducer } from 'app/entities/users/user.reducer';
import { UserManagementComponent } from './user-management.component';

describe('UserManagementComponent', () => {
  let component: UserManagementComponent;
  let fixture: ComponentFixture<UserManagementComponent>;

  beforeEach(async(() => {

    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'app-settings-sidebar' }),
        MockComponent({ selector: 'app-user-form', inputs: ['createUserForm'] }),
        MockComponent({ selector: 'app-user-table',
            inputs: [
              'addButtonText',
              'addDescription',
              'baseUrl',
              'users',
              'removeText',
              'showEmptyMessage',
              'showTable'
            ] }),
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
        }, { runtimeChecks })
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
});
