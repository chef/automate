import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, Validators, FormControl } from '@angular/forms';
import { Store, select } from '@ngrx/store';
import { identity, isNil } from 'lodash/fp';
import { combineLatest, Subject } from 'rxjs';
import { filter, pluck, takeUntil } from 'rxjs/operators';

import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams } from 'app/route.selectors';
import { Regex } from 'app/helpers/auth/regex';
import { pending, EntityStatus } from 'app/entities/entities';
import {
  GetNotification,
  UpdateNotification,
  TestNotification
} from 'app/entities/notification_rules/notification_rule.action';
import {
  notificationFromRoute,
  getStatus,
  updateStatus
} from 'app/entities/notification_rules/notification_rule.selectors';
import {
  NotificationRule,
  ServiceActionType,
  RuleType
} from 'app/entities/notification_rules/notification_rule.model';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

type NotificationTabName = 'details';

enum UrlTestState {
  Inactive,
  Loading,
  Success,
  Failure
}

@Component({
  selector: 'app-notification-details',
  templateUrl: './notification-details.component.html',
  styleUrls: ['./notification-details.component.scss']
})

export class NotificationDetailsComponent implements OnInit, OnDestroy {
  public tabValue: NotificationTabName = 'details';
  public notification = new NotificationRule('', '', null, '', null, '', false);
  public updateForm: FormGroup;
  public saveInProgress = false;
  public testInProgress = false;
  public saveSuccessful = false;
  public hookStatus = UrlTestState.Inactive;
  public notificationDetailsLoading = true;
  public targetKeys: string[];
  public alertTypeKeys: string[];
  public displayCriticalControlsBox = false;
  public targetTypeValue: string;
  public ruleTypeValue: string;
  private isDestroyed = new Subject<boolean>();

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private telemetryService: TelemetryService
  ) {

    this.updateForm = this.fb.group({
      // Must stay in sync with error checks in notification-details.component.html
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      targetType: [[]],
      ruleType: [[]],
      // Note that URL here may be FQDN -or- IP!
      targetUrl: ['', [Validators.required, Validators.pattern(Regex.patterns.VALID_FQDN)]]
    });

    this.store.select(routeParams).pipe(
      pluck('id'),
      filter(identity),
      takeUntil(this.isDestroyed))
      .subscribe((id: string) => {
        this.store.dispatch(new GetNotification({ id }));
      });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(notificationFromRoute)
    ]).pipe(
      filter(([status, notification]) =>
      status === EntityStatus.loadingSuccess && !isNil(notification)),
      takeUntil(this.isDestroyed))
      .subscribe(([_, notification]) => {
        this.notification = notification;
        this.targetTypeValue = notification.targetType;
        this.ruleTypeValue = notification.ruleType;
        setTimeout(() => (
          this.updateForm.controls.name.setValue(this.notification.name),
          this.updateForm.controls.targetUrl.setValue(this.notification.targetUrl),
          this.notificationDetailsLoading = false
        ),
        200);
      });

    this.store.pipe(
      select(updateStatus),
      filter(state => this.saveInProgress && !pending(state),
      takeUntil(this.isDestroyed)))
      .subscribe((state) => {
        this.saveInProgress = false;
        this.saveSuccessful = (state === EntityStatus.loadingSuccess);
        if (this.saveSuccessful) {
          this.updateForm.markAsPristine();
        }
      });
  }

  ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.targetKeys = this.notification.getTargetTypeKeys();
    this.alertTypeKeys = this.notification.getAlertTypeKeys();
  }

  public saveNotification(): void {
    this.saveSuccessful = false;
    this.saveInProgress = true;
    this.notification.name = this.updateForm.controls['name'].value.trim();
    this.notification.targetUrl = this.updateForm.controls['targetUrl'].value.trim();
    this.store.dispatch(new UpdateNotification({
      notification: this.notification
    }));
    this.targetTypeValue = this.notification.targetType;
    this.ruleTypeValue = this.notification.ruleType;
    this.telemetryService.track('Settings_Notifications_Details_Save');
  }

  public sendTestForTargetUrl(): void {
    this.testInProgress = true;
    const name: string = this.updateForm.controls['name'].value.trim();
    const targetUrl: string = this.updateForm.controls['targetUrl'].value.trim();
    const targetSecretID: string = this.notification.targetSecretId;
    this.store.dispatch(new TestNotification({ name, targetUrl, targetSecretID}));
    this.testInProgress = false;
    this.telemetryService.track('Settings_Notifications_Details_TestDataFeed');
  }

  public changeSelectionForWebhookType(event: { target: { value: ServiceActionType } }) {
    if ( this.notification.targetType !==  event.target.value ) {
      this.updateForm.controls.targetType.markAsDirty();
      this.notification.targetType = event.target.value;
      this.displayCriticalControlsBox = this.displayCriticalControlsCheckbox();
    }
  }

  public updateCriticalControlsOnly(event: boolean) {
    this.notification.criticalControlsOnly = event;
  }

  public displayCriticalControlsCheckbox() {
    return this.notification.targetType === ServiceActionType.SERVICENOW &&
      this.notification.ruleType === 'ComplianceFailure';
  }

  public setFailureType(event: { target: { value: RuleType } }) {
    if ( this.notification.ruleType !==  event.target.value ) {
      this.updateForm.controls.ruleType.markAsDirty();
    }

    this.notification.ruleType = event.target.value;
    this.displayCriticalControlsBox = this.displayCriticalControlsCheckbox();
    if (this.notification.ruleType !== 'ComplianceFailure' &&
      this.notification.targetType === ServiceActionType.SERVICENOW) {
        this.notification.criticalControlsOnly = false;
    }
  }

  public get nameCtrl(): FormControl {
    return <FormControl>this.updateForm.controls.name;
  }

  public get urlCtrl(): FormControl {
    return <FormControl>this.updateForm.controls.targetUrl;
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
