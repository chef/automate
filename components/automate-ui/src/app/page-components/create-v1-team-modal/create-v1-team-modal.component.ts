import { Component, EventEmitter, Input, Output, OnInit } from '@angular/core';
import { FormGroup } from '@angular/forms';
import { IdMapper } from 'app/helpers/auth/id-mapper';

@Component({
  selector: 'app-create-v1-team-modal',
  templateUrl: './create-v1-team-modal.component.html',
  styleUrls: ['./create-v1-team-modal.component.scss']
})
export class CreateV1TeamModalComponent implements OnInit {
  @Input() visible = false;
  @Input() creating = false;
  @Input() objectNoun: string;
  @Input() createForm: FormGroup; // NB: The form must contain 'name' and 'id' fields
  @Input() conflictErrorEvent: EventEmitter<boolean>; // TC: This element assumes 'id' is the
                                                      // only create field that can conflict.
  @Output() close = new EventEmitter();
  @Output() createClicked = new EventEmitter();

  // Whether the edit ID form is open or not.
  public modifyID = false;
  public conflictError = false;

  ngOnInit(): void {
    this.conflictErrorEvent.subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
      // Open the ID input on conflict so user can resolve it.
      this.modifyID = true;
    });
  }

  public handleNameInput(event: KeyboardEvent): void {
    if (!this.modifyID && !this.isNavigationKey(event)) {
      this.conflictError = false;
      this.createForm.controls['id'].setValue(
        IdMapper.transform(this.createForm.controls['name'].value.trim()));
    }
  }

  public handleIDInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  closeEvent(): void {
    this.modifyID = false;
    this.close.emit();
  }

  createObject(): void {
    this.createClicked.emit();
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }
}

