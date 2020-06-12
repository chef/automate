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
  private isDestroyed = new Subject<boolean>();
  notificationDetailsLoading = true;

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) {

    this.updateForm = this.fb.group({
      // Must stay in sync with error checks in data-feed-details.component.html
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      // Note that URL here may be FQDN -or- IP!
      targetUrl: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
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
        setTimeout(() => (
          this.updateForm.controls.name.setValue(this.notification.name),
          this.updateForm.controls.targetUrl.setValue(this.notification.targetUrl),
          this.notificationDetailsLoading = false
        ),
        200);
      });

    this.store.pipe(
      select(updateStatus),
      takeUntil(this.isDestroyed),
      filter(state => this.saveInProgress && !pending(state)))
      .subscribe((state) => {
        this.saveInProgress = false;
        this.saveSuccessful = (state === EntityStatus.loadingSuccess);
        if (this.saveSuccessful) {
          this.updateForm.markAsPristine();
        }
      });
  }

  public ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Settings);
  }

  public saveNotification(): void {
    this.saveSuccessful = false;
    this.saveInProgress = true;
    this.notification.name = this.updateForm.controls['name'].value.trim();
    this.notification.targetUrl = this.updateForm.controls['targetUrl'].value.trim();
    this.store.dispatch(new UpdateNotification({
      notification: this.notification, username: '', password: ''
    }));
  }

  public sendTestForTargetUrl(): void {
    this.testInProgress = true;
    const name: string = this.updateForm.controls['name'].value.trim();
    const targetUrl: string = this.updateForm.controls['targetUrl'].value.trim();
    const targetSecretID: string = this.notification.targetSecretId;
    this.store.dispatch(new TestNotification({ name, targetUrl, targetSecretID}));
    this.testInProgress = false;
  }

  public getAlertTypeKeys() {
    return this.notification.getAlertTypeKeys();
  }

  public getTagetTypeKeys() {
    return this.notification.getTargetTypeKeys();
  }

  public changeSelectionForWebhookType(event: { target: { value: ServiceActionType } }) {
    this.notification.targetType = event.target.value;
  }

  public updateCriticalControlsOnly(event: boolean) {
    this.notification.criticalControlsOnly = event;
  }

  public displayCriticalControlsCheckbox() {
    return this.notification.targetType === ServiceActionType.SERVICENOW &&
    this.notification.ruleType === 'ComplianceFailure';
  }

  public setFailureType(event: { target: { value: RuleType } }) {
    this.notification.ruleType = event.target.value;
    if (this.notification.ruleType !== 'ComplianceFailure' &&
    this.notification.targetType === ServiceActionType.SERVICENOW) {
      this.notification.criticalControlsOnly = false;
    }
  }

  public urlPresent() {
    return this.updateForm.controls.targetUrl.value !== '' ? true : false;
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
