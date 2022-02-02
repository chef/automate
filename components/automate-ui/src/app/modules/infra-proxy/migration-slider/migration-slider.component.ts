import { Component, HostBinding } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { Utilities } from 'app/helpers/utilities/utilities';

@Component({
  selector: 'app-migration-slider',
  templateUrl: './migration-slider.component.html',
  styleUrls: ['./migration-slider.component.scss']
})
export class MigrationSliderComponent {
  public migrating = false;
  public checkedUser = false;
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

  toggleSlide() {
    this.isSlideOpen1 = !this.isSlideOpen1;
  }

  migrationFile() {
    this.toggleSlide();
  }

  slidePanel() {
    console.log('migration part');
    this.isSlideOpen1 = true;
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
