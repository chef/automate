import { Component, EventEmitter, Input, Output, OnInit } from '@angular/core';
import { FormGroup } from '@angular/forms';

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
  @Input() createForm: FormGroup;
  @Input() hookStatus = UrlTestState.Inactive;

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

  // getAlertTypeKeys() {
  //   return this.rule.getAlertTypeKeys();
  // }

  // updateCriticalControlsOnly(event) {
  //   this.model.rule.criticalControlsOnly = event;
  // }

  // displayCriticalControlsCheckbox() {
  //   return this.model.rule.targetType === ServiceActionType.SERVICENOW &&
  //   this.model.rule.ruleType === 'ComplianceFailure';
  // }

  // onSelectionChange(name) {
  //   this.targetType = name;
  //   this.model.rule.targetType = name;
  // }

  // targetTypeChosen() {
  //   return typeof this.targetType !== 'undefined';
  // }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }
}

