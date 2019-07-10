import { Component, Input, Output, EventEmitter } from '@angular/core';

@Component({
  selector: 'app-confirm-apply-start-modal',
  templateUrl: './confirm-apply-start-modal.component.html',
  styleUrls: [ './confirm-apply-start-modal.component.scss' ]
})
export class ConfirmApplyStartModalComponent {

  @Input() visible = false;

  @Output() confirm: EventEmitter<void> = new EventEmitter();

  @Output() cancel: EventEmitter<void> = new EventEmitter();
}
