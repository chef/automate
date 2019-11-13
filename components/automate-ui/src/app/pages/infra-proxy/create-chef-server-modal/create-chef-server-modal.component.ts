import { Component, EventEmitter, Input, Output, OnInit } from '@angular/core';
import { FormGroup } from '@angular/forms';

@Component({
  selector: 'app-create-chef-server-modal',
  templateUrl: './create-chef-server-modal.component.html',
  styleUrls: ['./create-chef-server-modal.component.scss']
})
export class CreateChefServerModalComponent implements OnInit {
  @Input() visible = false;
  @Input() creating = false;
  @Input() createForm: FormGroup;
  @Input() conflictErrorEvent: EventEmitter<boolean>;
  @Output() close = new EventEmitter();
  @Output() createClicked = new EventEmitter();

  public conflictError = false;

  ngOnInit(): void {
  }

  public handleNameInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  public handleServerIdInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  public handleFqdnInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  closeEvent(): void {
    this.close.emit();
  }

  createChefServer(): void {
  }

  createObject(): void {
  }
  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }
}

