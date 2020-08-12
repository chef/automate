import { Component, EventEmitter, Input, Output, OnInit } from '@angular/core';
import { FormGroup } from '@angular/forms';
import {
  NotificationRule,
  ServiceActionType,
  RuleType
} from 'app/entities/notification_rules/notification_rule.model';

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
export class CreateNotificationModalComponent implements OnInit {
  @Input() visible = false;
  @Input() creating = false;
  @Input() sending = false;
  @Input() conflictErrorEvent: EventEmitter<boolean>;
  @Output() close = new EventEmitter();
  @Output() createClicked = new EventEmitter();
  @Output() sendTestClicked = new EventEmitter();
  @Output() targetTypeChanged = new EventEmitter();
  @Input() createForm: FormGroup;
  @Input() hookStatus = UrlTestState.Inactive;
  @Input() notificationRule = new NotificationRule('', '', null, '', null, '', false);
  @Input() targetKeysValue: string[];
  @Input() alertTypeKeysValue: string[];
  public conflictError = false;
  public urlState = UrlTestState;

  ngOnInit() {
    this.conflictErrorEvent.subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
    });
  }

  public handleInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  closeEvent(): void {
    this.close.emit();
  }

  createNotification(): void {
    this.createClicked.emit();
  }

  sendTest() {
    this.sendTestClicked.emit();
  }

  urlPresent() {
    return this.createForm.controls.targetUrl.value !== '' ? true : false;
  }

  getAlertTypeKeys() {
    return this.notificationRule.getAlertTypeKeys();
  }

  getTagetTypeKeys() {
    return this.notificationRule.getTargetTypeKeys();
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
    this.targetTypeChanged.emit();
  }

  updateCriticalControlsOnly(event: boolean) {
    this.notificationRule.criticalControlsOnly = event;
  }

  displayCriticalControlsCheckbox() {
    return this.createForm.value.targetType === ServiceActionType.SERVICENOW &&
    this.notificationRule.ruleType === 'ComplianceFailure';
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }
}

