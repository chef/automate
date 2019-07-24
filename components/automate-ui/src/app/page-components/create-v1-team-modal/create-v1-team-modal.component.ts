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

  public modifyName = false;
  public conflictError = false;

  ngOnInit(): void {
    this.conflictErrorEvent.subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
      // Open the Name input on conflict so user can resolve it.
      this.modifyName = true;
    });
  }

  public handleNameInput(event: KeyboardEvent): void {
    if (!this.modifyName && !this.isNavigationKey(event)) {
      this.conflictError = false;
    }
  }

  public handleDescriptionInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  closeEvent(): void {
    this.modifyName = false;
    this.close.emit();
  }

  createV1Team(): void {
    this.createClicked.emit();
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }
}

