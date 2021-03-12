import { Component, EventEmitter, Input, Output, OnInit } from '@angular/core';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { FormGroup } from '@angular/forms';

@Component({
  selector: 'app-create-chef-server-modal',
  templateUrl: './create-chef-server-modal.component.html',
  styleUrls: ['./create-chef-server-modal.component.scss']
})
export class CreateChefServerModalComponent implements OnInit {
  @Input() visible = false;
  @Input() creating = false;
  @Input() conflictErrorEvent: EventEmitter<boolean>;
  @Output() close = new EventEmitter();
  @Output() createClicked = new EventEmitter();
  @Input() createForm: FormGroup;

  public modifyID = false; // Whether the edit ID form is open or not.

  public conflictError = false;

  ngOnInit() {
    this.conflictErrorEvent.subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
      // Open the ID input on conflict so user can resolve it.
      this.modifyID = isConflict;
    });
  }

  handleNameInput(event: KeyboardEvent): void {
    if (!this.modifyID && !this.isNavigationKey(event)) {
      this.conflictError = false;
      this.createForm.controls.id.setValue(
        IdMapper.transform(this.createForm.controls.name.value.trim()));
    }
  }

  public handleInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  closeEvent(): void {
    this.modifyID = false;
    this.close.emit();
  }

  createChefServer(): void {
    this.createClicked.emit();
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }
}
