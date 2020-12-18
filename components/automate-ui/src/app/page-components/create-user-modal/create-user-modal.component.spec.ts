import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { CreateUserModalComponent } from './create-user-modal.component';
import { EventEmitter } from '@angular/core';
import { using } from 'app/testing/spec-helpers';

describe('CreateUserModalComponent', () => {
  let component: CreateUserModalComponent;
  let fixture: ComponentFixture<CreateUserModalComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-modal', inputs: ['visible'] }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-toolbar' }),
        CreateUserModalComponent
      ],
      imports: [
        ReactiveFormsModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateUserModalComponent);
    component = fixture.componentInstance;
    component.openEvent = new EventEmitter();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

 describe('#createUserForm', () => {
    it('should be invalid when no fields are filled out', () => {
      expect(component.createUserForm.valid).toBeFalsy();
    });

     it('should be valid when all fields are filled out and passwords match', () => {
      component.createUserForm.controls['displayName'].setValue('Sam');
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

  using([
    ['my-name', 'my-name', 'mirrors input with no leading/trailing whitespace'],
    ['  my-name  ', 'my-name', 'trims leading whitespace'],
    ['my-name  ', 'my-name', 'trims trailing whitespace']
  ], function (inputVal: string, outputVal: string, desc: string) {

    it('handleNameInput ' + desc, () => {
      component.createUserForm.controls['displayName'].setValue(inputVal);
      component.handleNameInput(new KeyboardEvent('keyup',
        // Typescript bug requires us to work around with <any>
        // https://github.com/Microsoft/TypeScript/issues/15228
        <any>{bubbles : true, cancelable : true, key : inputVal, char : inputVal, shiftKey : true
      }));
      expect(component.createUserForm.controls['username'].value).toBe(outputVal);
    });
  });

});
