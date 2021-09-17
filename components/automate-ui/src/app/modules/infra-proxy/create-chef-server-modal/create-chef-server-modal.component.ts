import { Component, EventEmitter, Input, Output, OnInit, HostBinding } from '@angular/core';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { FormGroup } from '@angular/forms';
import { Utilities } from 'app/helpers/utilities/utilities';

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
  @Input() fqdnForm: FormGroup;
  @Input() ipForm: FormGroup;
  @HostBinding('class.active') isSlideOpen = false;

  public modifyID = false; // Whether the edit ID form is open or not.

  public conflictError = false;
  public selected = 'fqdn';

  ngOnInit() {
    this.conflictErrorEvent.subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
      // Open the ID input on conflict so user can resolve it.
      this.modifyID = isConflict;
    });
  }

  handleNameInput(event: KeyboardEvent): void {
    if (!this.modifyID && !Utilities.isNavigationKey(event)) {
      this.conflictError = false;
      this.createForm.controls.id.setValue(
        IdMapper.transform(this.createForm.controls.name.value.trim()));
    }
  }

  updateFormDisplay(id: string): void {
    this.selected = id;
  }

  public handleInput(event: KeyboardEvent): void {
    if (Utilities.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  createChefServer(): void {
    this.toggleSlide();
    this.createClicked.emit();
    this.modifyID = false;
  }

  closeSeverSlider() {
    this.toggleSlide();
    this.modifyID = false;
    this.close.emit();
  }

  toggleSlide() {
    this.isSlideOpen = !this.isSlideOpen;
  }

  slidePanel() {
    this.isSlideOpen = true;
  }
}
