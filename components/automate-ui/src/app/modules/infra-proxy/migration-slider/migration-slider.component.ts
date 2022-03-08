import { Component, EventEmitter, HostBinding, Input, OnChanges, Output } from '@angular/core';
import { FormBuilder, FormControl, FormGroup } from '@angular/forms';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { Utilities } from 'app/helpers/utilities/utilities';
import { User } from '../../../entities/orgs/org.model';
// import { CheckUser } from '../../../entities/orgs/org.actions';

@Component({
  selector: 'app-migration-slider',
  templateUrl: './migration-slider.component.html',
  styleUrls: ['./migration-slider.component.scss']
})
export class MigrationSliderComponent implements OnChanges {
  @Input() migrationID: string;
  @Input() isPreview: boolean;
  @Input() previewData;
  @Output() confirmPreview = new EventEmitter();
  @Output() cancelMigration = new EventEmitter();

  public checkedUser = false;
  public migrationForm: FormGroup;
  public usersData: User[];

  @HostBinding('class.active') isSlideOpen1 = false;
  store: any;

  constructor(
    private fb: FormBuilder
  ) {
    this.migrationForm = this.fb.group({});
  }

  ngOnChanges() {
    let group={};
    if(this.previewData) {
      this.previewData.users.forEach((input_template: { automate_username: string | number; })=>{
        group[input_template.automate_username]=new FormControl('');  
      })
    }
    this.migrationForm = new FormGroup(group)
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
    this.previewData.users.forEach((item: { selected: any; }) => item.selected = checked);
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

  handleName(event: KeyboardEvent ,i: number): void {
    console.log((event.target as HTMLInputElement).value, i);
    // this.store.dispatch(new CheckUser(event.target as HTMLInputElement).value);
  }
}
