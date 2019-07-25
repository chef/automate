import { Component, EventEmitter, Input, Output, OnInit } from '@angular/core';
import { FormGroup } from '@angular/forms';

@Component({
  selector: 'app-create-v1-team-modal',
  templateUrl: './create-v1-team-modal.component.html',
  styleUrls: ['./create-v1-team-modal.component.scss']
})
export class CreateV1TeamModalComponent implements OnInit {
  @Input() visible = false;
  @Input() creating = false;
  @Input() createForm: FormGroup;
  @Input() conflictErrorEvent: EventEmitter<boolean>;

  @Output() close = new EventEmitter();
  @Output() createClicked = new EventEmitter();

  public conflictError = false;

  ngOnInit(): void {
    this.conflictErrorEvent.subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
    });
  }

  public handleNameInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  public handleDescriptionInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  closeEvent(): void {
    this.close.emit();
  }

  createV1Team(): void {
    this.createClicked.emit();
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }
}

