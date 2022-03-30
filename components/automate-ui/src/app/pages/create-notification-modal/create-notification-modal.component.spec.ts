import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';
import { MockComponent } from 'ng2-mock-component';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { CreateNotificationModalComponent } from './create-notification-modal.component';
import { EventEmitter } from '@angular/core';
import { NotificationRuleRequests } from 'app/entities/notification_rules/notification_rule.requests';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { HttpStatus } from 'app/types/types';
import { MatSelectModule } from '@angular/material/select';
import {
  CreateNotificationRuleSuccess,
  CreateNotificationRuleFailure
} from 'app/entities/notification_rules/notification_rule.action';
import {
  NotificationRule,
  ServiceActionType
} from 'app/entities/notification_rules/notification_rule.model';


describe('CreateNotificationModalComponent', () => {
  let component: CreateNotificationModalComponent;
  let fixture: ComponentFixture<CreateNotificationModalComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-modal', inputs: ['visible'] }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-toolbar' }),
        CreateNotificationModalComponent
      ],
      imports: [
        MatSelectModule,
        ReactiveFormsModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      providers: [
        NotificationRuleRequests,
        HttpClient,
        HttpHandler
      ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateNotificationModalComponent);
    component = fixture.componentInstance;
    component.openEvent = new EventEmitter();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('#createNotificationForm', () => {
    let store: Store<NgrxStateAtom>;
    const notification: NotificationRule =
      new NotificationRule('id1', 'test rule1', 'ComplianceFailure',
        'http://foo.com', ServiceActionType.SLACK, '', false);

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('should be invalid when no fields are filled out', () => {
      expect(component.createForm.valid).toBeFalsy();
    });

     it('should be valid when all fields are filled out', () => {
      component.createForm.controls['name'].setValue(notification.name);
      component.createForm.controls['targetType'].setValue(notification.targetType);
      component.createForm.controls['ruleType'].setValue(notification.ruleType);
      component.createForm.controls['targetUrl'].setValue(notification.targetUrl);
      expect(component.createForm.valid).toBeTruthy();
    });

    it('opening create modal resets name, targetType, RuleType and targetUrl to empty string',
      () => {
      component.visible = true;
      expect(component.createForm.controls['name'].value).toEqual('');
      expect(component.createForm.controls['targetType'].value).toEqual('');
      expect(component.createForm.controls['ruleType'].value).toEqual('');
      expect(component.createForm.controls['targetUrl'].value).toEqual('');
    });

    it('hide modal after create a notification.', () => {
      component.createForm.controls['name'].setValue(notification.name);
      component.createForm.controls['targetType'].setValue(notification.targetType);
      component.createForm.controls['ruleType'].setValue(notification.ruleType);
      component.createForm.controls['targetUrl'].setValue(notification.targetUrl);
      component.createNotification();

      store.dispatch(new CreateNotificationRuleSuccess(notification));
      expect(component.visible).toBe(false);
    });

    it('on create error, modal is closed with failure banner', () => {
      component.createForm.controls['name'].setValue(notification.name);
      component.createForm.controls['targetType'].setValue(notification.targetType);
      component.createForm.controls['ruleType'].setValue(notification.ruleType);
      component.createForm.controls['targetUrl'].setValue(notification.targetUrl);
      component.createNotification();

      const error = <HttpErrorResponse>{
        status: HttpStatus.INTERNAL_SERVER_ERROR,
        ok: false
      };
      store.dispatch(new CreateNotificationRuleFailure(error));
      expect(component.conflictError).toBe(false);
    });
  });

  describe('create notification form validation', () => {
    it('- url field validity', () => {
      component.visible = true;

      let errors = {};
      const url = component.createForm.controls['targetUrl'];
      expect(url.valid).toBeFalsy();

      // url field is required
      errors = url.errors || {};
      expect(errors['required']).toBeTruthy();

      url.setValue('');
      errors = url.errors || {};
      expect(errors['required']).toBeTruthy();

      // Set url to invalid inputs
      url.setValue('  ');
      errors = url.errors || {};
      expect(errors['pattern']).toBeTruthy();
      expect(errors['required']).toBeFalsy();

      // Set url to valid inputs
      url.setValue('any');
      errors = url.errors || {};
      expect(errors['pattern']).toBeFalsy();
    });
  });
});
