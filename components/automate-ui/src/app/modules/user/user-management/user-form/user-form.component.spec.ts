import { EventEmitter } from '@angular/core';
import { ReactiveFormsModule, FormBuilder } from '@angular/forms';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';

import { using } from 'app/testing/spec-helpers';
import { UserFormComponent } from './user-form.component';

describe('UserFormComponent', () => {
  let fixture: ComponentFixture<UserFormComponent>;
  let component: UserFormComponent;

  beforeEach(async () => {
    TestBed.configureTestingModule({
      declarations: [
        UserFormComponent,
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-toolbar' })
      ],
     imports: [
        ReactiveFormsModule
      ]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(UserFormComponent);
    component = fixture.componentInstance;
    component.conflictErrorEvent = new EventEmitter();
    component.passwordErrorEvent = new EventEmitter();
    component.closeEvent = new EventEmitter();
    component.createUserForm = new FormBuilder().group({
      displayName: ['', null],
      username: ['', null],
      password: ['', null],
      confirmPassword: ['', null]
    });
  });

  it('should create', () => {
    expect(component).toBeTruthy();
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
