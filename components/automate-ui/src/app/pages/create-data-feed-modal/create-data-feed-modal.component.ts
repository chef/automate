import { Component, EventEmitter, Input, Output, OnInit } from '@angular/core';
import { FormGroup } from '@angular/forms';

enum UrlTestState {
  Inactive,
  Loading,
  Success,
  Failure
}

@Component({
  selector: 'app-create-data-feed-modal',
  templateUrl: './create-data-feed-modal.component.html',
  styleUrls: ['./create-data-feed-modal.component.scss']
})
export class CreateDataFeedModalComponent implements OnInit {
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

  createDataFeed(): void {
    this.createClicked.emit();
  }

  sendTest() {
    this.sendTestClicked.emit();
  }

  urlPresent() {
    return this.createForm.controls.url.value !== '' ? true : false;
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }
}

