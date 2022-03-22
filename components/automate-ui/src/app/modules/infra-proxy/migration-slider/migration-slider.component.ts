import { Component, EventEmitter, HostBinding, Input, OnChanges, Output } from '@angular/core';
import { FormBuilder, FormControl, FormGroup } from '@angular/forms';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { Utilities } from 'app/helpers/utilities/utilities';
import { User } from '../../../entities/orgs/org.model';
import { CheckUser } from '../../../entities/orgs/org.actions';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';

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
  public selectedUsersData: User[] = [];
  public conflictedUsers: User[] = [];
  @HostBinding('class.active') isSlideOpen1 = false;

  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>
  ) {
    this.migrationForm = this.fb.group({});
  }

  ngOnChanges() {
    const group = {};
    if (this.previewData) {
      this.previewData.staged_data.users.forEach((input_template: { automate_username: string | number; }) => {
        group[input_template.automate_username] = new FormControl('');
      });

      this.usersData = this.previewData.staged_data.users.map((obj: User)=> ({ ...obj, is_seleted: false }))
      this.conflictedUsers = this.usersData.filter((obj: User)=> obj.is_conflicting)
      this.usersData.forEach((item: User) => item.is_selected = true)
      this.usersData.forEach((item: User) => this.selectedUsersData.push(item))
    }
    this.migrationForm = new FormGroup(group);
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
    this.usersData.forEach((item: User) => item.is_selected = false);
  }

  confirmPreviewData() {
    this.toggleSlide();
    this.confirmPreview.emit(this.selectedUsersData);
  }

  slidePanel() {
    this.isSlideOpen1 = true;
  }

  selectedAllUsers(event: any) {
    const checked = event.target.checked;
    this.usersData.forEach((item: User) => item.is_selected = checked);
    if (checked) {
      this.usersData.forEach((item: User) => this.selectedUsersData.push(item))
    } else {
      this.selectedUsersData = []
    }
  }

  selectedUser(event: any) {
    const index = this.usersData.findIndex(x => x.username === event.target.value);
    if (event.target.checked) {
      this.selectedUsersData.push(this.usersData[index]);
    } else {
      this.selectedUsersData.forEach((value, index)=>{
        if (value.username == event.target.value) this.selectedUsersData.splice(index,1);
      });
    }
  }

  handleNameInput(event: KeyboardEvent): void {
    if (!Utilities.isNavigationKey(event)) {
      this.migrationForm.controls.name.setValue(
        IdMapper.transform(this.migrationForm.controls.name.value.trim()));
    }
  }

  handleName(event: KeyboardEvent): void {
    if ((event.target as HTMLInputElement).value !== '') {
      const payload = {
        user: (event.target as HTMLInputElement).value
      };
      this.store.dispatch(new CheckUser(payload));
    }
  }

  onChangeEvent(event: any){
    const username = event.target.dataset.username
    const index = this.usersData.findIndex(x => x.username === username);
    this.usersData[index].automate_username = event.target.value
  }
}
