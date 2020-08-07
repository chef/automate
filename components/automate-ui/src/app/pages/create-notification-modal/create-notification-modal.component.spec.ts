import { EventEmitter } from '@angular/core';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ReactiveFormsModule, FormBuilder } from '@angular/forms';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';

import { CreateNotificationModalComponent } from './create-notification-modal.component';

describe('CreateNotificationModalComponent', () => {
  let component: CreateNotificationModalComponent;
  let fixture: ComponentFixture<CreateNotificationModalComponent>;

  beforeEach(async () => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-modal',
          inputs: ['visible'],
          outputs: ['close']
        }),
        CreateNotificationModalComponent
      ],
      imports: [
        ReactiveFormsModule
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateNotificationModalComponent);
    component = fixture.componentInstance;
    component.createForm = new FormBuilder().group({
      id: ['', null],
      name: ['', null],
      url: ['', null],
      targetType: ['', null],
      ruleType: ['', null],
      username: ['', null],
      password: ['', null]
    });
    component.conflictErrorEvent = new EventEmitter();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
