import { Component, EventEmitter, HostBinding, Input, Output } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { Utilities } from 'app/helpers/utilities/utilities';
import { User } from '../../../entities/orgs/org.model';

@Component({
  selector: 'app-migration-slider',
  templateUrl: './migration-slider.component.html',
  styleUrls: ['./migration-slider.component.scss']
})
export class MigrationSliderComponent {
  @Input() migrationID: string;
  @Input() isPreview: boolean;
  @Input() previewData;
  @Output() confirmPreview = new EventEmitter();
  @Output() cancelMigration = new EventEmitter();

  public checkedUser = false;
  public migrationForm: FormGroup;
  public usersData: User[];

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

  confirmPreviewData() {
    this.toggleSlide();
    this.confirmPreview.emit(this.migrationID);
  }

  slidePanel() {
    this.isSlideOpen1 = true;
  }

  selectedAllUsers(event: any) {
    const checked = event.target.checked;
    this.previewData.users.forEach(item => item.selected = checked);
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
