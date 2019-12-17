import { Component, Input, Output, EventEmitter } from '@angular/core';

@Component({
  selector: 'app-message-modal',
  templateUrl: './message-modal.component.html',
  styleUrls: ['./message-modal.component.scss']
})
export class MessageModalComponent {
  @Input() visible = false;
  @Input() title = '';
  @Output() close = new EventEmitter();

  closeEvent(): void {
    this.close.emit();
  }
}
