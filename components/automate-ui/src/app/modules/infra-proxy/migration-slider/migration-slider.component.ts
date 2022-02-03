import { Component, EventEmitter, HostBinding, Input, Output } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { Utilities } from 'app/helpers/utilities/utilities';

@Component({
  selector: 'app-migration-slider',
  templateUrl: './migration-slider.component.html',
  styleUrls: ['./migration-slider.component.scss']
})
export class MigrationSliderComponent {
  @Input() migrationID: string;
  @Input() previewData;
  @Output() migrationPreview = new EventEmitter();
  @Output() cancelMigration = new EventEmitter();

  public migrating = false;
  public checkedUser = false;
  public isLDPUser = true;
  public isEditable = true;
  public migrationForm: FormGroup;

  @HostBinding('class.active') isSlideOpen1 = false;

  constructor(
    private fb: FormBuilder
  ) {
    this.migrationForm = this.fb.group({
      name: ['']
    });
  }

  closeMigrationSlider() {
    this.toggleSlide();
  }

  cancelMigrationclicked() {
    this.toggleSlide();
    this.cancelMigration.emit(this.migrationID);
  }

  toggleSlide() {
    this.isSlideOpen1 = !this.isSlideOpen1;
  }

  migrationFile() {
    this.toggleSlide();
    this.migrationPreview.emit(this.migrationForm);
  }

  slidePanel() {
    this.isSlideOpen1 = true;
  }

  selectedAllUsers(event: any) {
    const checked = event.target.checked;
    console.log(checked);
    // this.users.forEach(item => item.selected = checked);
  }

  selectedUser(value: boolean) {
    console.log(value);
  }

  handleNameInput(event: KeyboardEvent): void {
    if (!Utilities.isNavigationKey(event)) {
      this.migrationForm.controls.name.setValue(
        IdMapper.transform(this.migrationForm.controls.name.value.trim()));
    }
  }
}
