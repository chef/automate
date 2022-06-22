import {
  Component,
  EventEmitter,
  Input,
  OnDestroy,
  OnInit
} from '@angular/core';
import { combineLatest, Subject } from 'rxjs';
import { Store, select } from '@ngrx/store';
import { FormBuilder,  Validators, FormGroup } from '@angular/forms';
import { first, filter, takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import { EntityStatus, pending } from 'app/entities/entities';
import { HttpStatus } from 'app/types/types';
import { Utilities } from 'app/helpers/utilities/utilities';
import {
  saveStatus,
  saveError
} from 'app/entities/notification_rules/notification_rule.selectors';
import {
  NotificationRule,
  ServiceActionType,
  RuleType
} from 'app/entities/notification_rules/notification_rule.model';
import {
  CreateNotificationRule
} from 'app/entities/notification_rules/notification_rule.action';
import {
  NotificationRuleRequests
} from 'app/entities/notification_rules/notification_rule.requests';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

enum UrlTestState {
  Inactive,
  Loading,
  Success,
  Failure
}

@Component({
  selector: 'app-create-notification-modal',
  templateUrl: './create-notification-modal.component.html',
  styleUrls: ['./create-notification-modal.component.scss']
})
export class CreateNotificationModalComponent implements OnInit, OnDestroy {
  @Input() openEvent: EventEmitter<void>;

  public visible = false;
  public creating = false;
  public sending = false;
  public conflictErrorEvent = false;
  public close = new EventEmitter();
  public createForm: FormGroup;
  public hookStatus = UrlTestState.Inactive;
  public notificationRule = new NotificationRule('', '', null, '', null, '', false);
  public conflictError = false;
  public urlState = UrlTestState;
  public targetKeys: string[];
  public alertTypeKeys: string[];
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private fb: FormBuilder,
    private notificationRuleRequests: NotificationRuleRequests,
    private telemetryService: TelemetryService
  ) {
    this.createForm = this.fb.group({
      // Must stay in sync with error checks in create-notification-modal.component.html
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      ruleType: ['', [Validators.required]],
      targetType: ['', [Validators.required]],

      // Note that URL here may be FQDN -or- IP!
      targetUrl: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      username: [[]],
      password: [[]]
    });

    this.targetKeys = this.notificationRule.getTargetTypeKeys();
    this.alertTypeKeys = this.notificationRule.getAlertTypeKeys();
  }

  ngOnInit() {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.conflictError = false;
        this.visible = true;
      });

    this.store.pipe(
      select(saveStatus),
      takeUntil(this.isDestroyed),
      filter(state => this.visible && !pending(state)))
      .subscribe(state => {
        this.creating = false;
        if (state === EntityStatus.loadingSuccess) {
          this.closeCreateModal();
          this.hookStatus = UrlTestState.Inactive;
        }
      });

    combineLatest([
      this.store.select(saveStatus),
      this.store.select(saveError)
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(() => this.visible),
      filter(([state, error]) => state === EntityStatus.loadingFailure && !isNil(error)))
      .subscribe(([_, error]) => {
        if (error.status === HttpStatus.CONFLICT) {
          this.conflictErrorEvent = true;
        } else {
          // Close the modal on any error other than conflict and display in banner.
          this.closeCreateModal();
          this.hookStatus = UrlTestState.Inactive;
        }
      });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public handleInput(event: KeyboardEvent): void {
    if (Utilities.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  closeCreateModal(): void {
    this.resetCreateModal();
    this.visible = false;
  }

  createNotification(): void {
    this.creating = true;
    this.notificationRule.name = this.createForm.value.name.trim();
    this.notificationRule.targetUrl = this.createForm.value.targetUrl.trim();
    const username: string = this.createForm.value.username || '';
    const password: string = this.createForm.value.password || '';
    this.store.dispatch(new CreateNotificationRule(this.notificationRule, username, password));
    this.telemetryService.track('Settings_Notifications_Create');
  }

  private resetCreateModal(): void {
    this.hookStatus = UrlTestState.Inactive;
    this.creating = false;
    this.createForm.reset();
    this.conflictErrorEvent = false;
  }

  sendTest() {
    this.sending = true;
    this.hookStatus = UrlTestState.Loading;
    const targetUrl: string =  this.createForm.controls.targetUrl.value;
    const targetUsername: string = this.createForm.controls.username.value || '';
    const targetPassword: string = this.createForm.controls.password.value || '';
    if (targetUrl && targetUsername.length && targetPassword.length) {
      this.notificationRuleRequests.testHookWithUsernamePassword(
        targetUrl, targetUsername, targetPassword
        ).pipe(first())
        .subscribe(
          () => this.revealUrlStatus(UrlTestState.Success),
          () => this.revealUrlStatus(UrlTestState.Failure)
        );
    } else {
      this.notificationRuleRequests.testHookWithNoCreds(targetUrl)
        .pipe(first())
        .subscribe(
          () => this.revealUrlStatus(UrlTestState.Success),
          () => this.revealUrlStatus(UrlTestState.Failure)
        );
    }
    this.sending = false;
    this.telemetryService.track('Settings_Notifications_CreateTestNotification');
  }

  public updateTargetType(): void {
    if (this.createForm.value.targetType === 'ServiceNowAlert') {
      this.createForm.get('username').setValidators([
        Validators.required
      ]);
      this.createForm.get('password').setValidators([
        Validators.required
      ]);
    } else {
      this.createForm.get('username').clearValidators();
      this.createForm.get('password').clearValidators();
    }
  }

  private revealUrlStatus(status: UrlTestState) {
    this.hookStatus = status;
  }

  urlPresent() {
    return this.createForm.controls.targetUrl.value !== '';
  }

  setFailureType(event: { value: RuleType }) {
    this.notificationRule.ruleType = event.value;
    this.createForm.value.ruleType = event.value;
    if (this.notificationRule.ruleType !== 'ComplianceFailure' &&
    this.notificationRule.targetType === ServiceActionType.SERVICENOW) {
      this.notificationRule.criticalControlsOnly = false;
    }
  }

  changeSelectionForWebhookType(event: { value: ServiceActionType }) {
    this.notificationRule.targetType = event.value;
    this.createForm.value.targetType = event.value;
    this.updateTargetType();
  }

  updateCriticalControlsOnly(event: boolean) {
    this.notificationRule.criticalControlsOnly = event;
  }

  displayCriticalControlsCheckbox() {
    return this.createForm.value.targetType === ServiceActionType.SERVICENOW &&
    this.notificationRule.ruleType === 'ComplianceFailure';
  }

}

