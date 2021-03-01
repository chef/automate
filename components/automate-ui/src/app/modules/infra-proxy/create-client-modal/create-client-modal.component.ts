import { Component, EventEmitter, Input, Output, OnInit } from '@angular/core';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { FormGroup } from '@angular/forms';


@Component({
  selector: 'app-create-client-modal',
  templateUrl: './create-client-modal.component.html',
  styleUrls: ['./create-client-modal.component.scss']
})
export class CreateClientModalComponent implements OnInit {
  @Input() visible = false;
  @Input() creating = false;
  @Input() conflictErrorEvent: EventEmitter<boolean>;
  @Output() close = new EventEmitter();
  @Output() createClicked = new EventEmitter();
  @Input() created = new EventEmitter();
  // @Input() checked = new EventEmitter();
  @Input() privateKey = new EventEmitter();
  @Input() createForm: FormGroup;
  @Output() onDownloadKey = new EventEmitter();
  
  public conflictError = false;
  public checkedValidaotr = false;

  ngOnInit() {
    this.conflictErrorEvent.subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
    });
  }

  handleNameInput(event: KeyboardEvent): void {
    if (!this.isNavigationKey(event)) {
      this.conflictError = false;
      this.createForm.controls.name.setValue(
        IdMapper.transform(this.createForm.controls.name.value.trim()));
    }
  }

  updateValidatorCheckbox(event: boolean): void {
    this.checkedValidaotr = event;
    this.createForm.controls.validator.setValue(this.checkedValidaotr);
  }

  // public handleInput(event: KeyboardEvent): void {
  //   if (this.isNavigationKey(event)) {
  //     return;
  //   }
  //   this.conflictError = false;
  // }

  closeEvent(): void {
    this.close.emit();
  }

  createClient(): void {
    this.createClicked.emit();
  }

  downloadKey() {
    console.log('download clicked');
    this.onDownloadKey.emit();
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }

}
