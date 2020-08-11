import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpErrorResponse } from '@angular/common/http';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { NotificationsComponent } from './notifications.component';
import { NotificationRuleRequests } from 'app/entities/notification_rules/notification_rule.requests';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { NotificationRule, ServiceActionType } from 'app/entities/notification_rules/notification_rule.model';
import { CreateNotificationRuleSuccess, CreateNotificationRuleFailure } from 'app/entities/notification_rules/notification_rule.action';
import { HttpStatus } from 'app/types/types';
import { TelemetryService } from '../../services/telemetry/telemetry.service';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';

class MockTelemetryService {
  track() { }
}

describe('NotificationsComponent', () => {
  let telemetryService: TelemetryService;
  let component: NotificationsComponent;
  let fixture: ComponentFixture<NotificationsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [
        NotificationsComponent,
        MockComponent({
        selector: 'app-create-notification-modal',
        inputs: ['visible', 'creating', 'conflictErrorEvent', 'createForm'],
        outputs: ['close', 'createClicked']
        }),
        MockComponent({ selector: 'app-delete-object-modal',
        inputs: ['default', 'visible', 'objectNoun', 'objectName'],
        outputs: ['close', 'deleteClicked'] }),
        MockComponent({ selector: 'chef-button',
                inputs: ['disabled', 'routerLink'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'a', inputs: ['routerLink'] })
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        FeatureFlagsService,
        NotificationRuleRequests,
        HttpClient,
        HttpHandler
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(NotificationsComponent);
    component = fixture.componentInstance;
    telemetryService = TestBed.inject(TelemetryService);
    fixture.detectChanges();
  });

  describe('sending telemetry', () => {
    beforeEach(() => {
      spyOn(telemetryService, 'track');
    });

    it('ensure telemetry is sent on deleting a rule', () => {
      component.deleteNotification();

      expect(telemetryService.track).toHaveBeenCalled();
    });

  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('create notification', () => {
    let store: Store<NgrxStateAtom>;

    const notification: NotificationRule =
      new NotificationRule('id1', 'test rule1', 'ComplianceFailure',
            'http://foo.com', ServiceActionType.SLACK, '', false);

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('openCreateModal opens modal', () => {
      expect(component.createModalVisible).toBe(false);
      component.openCreateModal();
      expect(component.createModalVisible).toBe(true);
    });

    it('opening create modal resets name, targetType, RuleType and targetUrl to empty string',
      () => {
      component.createNotificationForm.controls['name'].setValue('any');
      component.createNotificationForm.controls['targetType'].setValue('any');
      component.createNotificationForm.controls['ruleType'].setValue('any');
      component.createNotificationForm.controls['targetUrl'].setValue('any');
      component.openCreateModal();
      expect(component.createNotificationForm.controls['name'].value).toBe(null);
      expect(component.createNotificationForm.controls['targetType'].value).toBe(null);
      expect(component.createNotificationForm.controls['ruleType'].value).toBe(null);
      expect(component.createNotificationForm.controls['targetUrl'].value).toBe(null);
    });

    it('on success, closes modal and adds new notification', () => {
      component.createNotificationForm.controls['name'].setValue(notification.name);
      component.createNotificationForm.controls['targetType'].setValue(notification.targetType);
      component.createNotificationForm.controls['ruleType'].setValue(notification.ruleType);
      component.createNotificationForm.controls['targetUrl'].setValue(notification.targetUrl);
      component.createNotification();

      store.dispatch(new CreateNotificationRuleSuccess(notification));
      component.rules$.subscribe(rules => {
        component.closeCreateModal();
        expect(rules).toContain(notification);
      });
    });

    it('on conflict error, modal is open with conflict error', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.openCreateModal();
      component.createNotificationForm.controls['name'].setValue(notification.name);
      component.createNotificationForm.controls['targetType'].setValue(notification.targetType);
      component.createNotificationForm.controls['ruleType'].setValue(notification.ruleType);
      component.createNotificationForm.controls['targetUrl'].setValue(notification.targetUrl);
      component.createNotification();

      const conflict = <HttpErrorResponse>{
        status: HttpStatus.CONFLICT,
        ok: false
      };
      store.dispatch(new CreateNotificationRuleFailure(conflict));

      expect(component.createModalVisible).toBe(true);
      expect(component.conflictErrorEvent.emit).toHaveBeenCalled();
    });

    it('on create error, modal is closed with failure banner', () => {
      spyOn(component.conflictErrorEvent, 'emit');
      component.openCreateModal();
      component.createNotificationForm.controls['name'].setValue(notification.name);
      component.createNotificationForm.controls['targetType'].setValue(notification.targetType);
      component.createNotificationForm.controls['ruleType'].setValue(notification.ruleType);
      component.createNotificationForm.controls['targetUrl'].setValue(notification.targetUrl);
      component.createNotification();

      const error = <HttpErrorResponse>{
        status: HttpStatus.INTERNAL_SERVER_ERROR,
        ok: false
      };
      store.dispatch(new CreateNotificationRuleFailure(error));

      expect(component.createModalVisible).toBe(false);
      expect(component.conflictErrorEvent.emit).toHaveBeenCalledWith(false);
    });

  });

  describe('create notification form validation', () => {

    it('- url field validity', () => {
      component.openCreateModal();

      let errors = {};
      const url = component.createNotificationForm.controls['targetUrl'];
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
