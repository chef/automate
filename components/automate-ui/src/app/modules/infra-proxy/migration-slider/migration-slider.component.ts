import { Component, EventEmitter, HostBinding, Input, OnChanges, OnDestroy, Output } from '@angular/core';
import { FormBuilder, FormControl, FormGroup } from '@angular/forms';
import { Subject } from 'rxjs';
import { pending, EntityStatus } from 'app/entities/entities';
import { filter, takeUntil } from 'rxjs/operators';
import { User } from '../../../entities/orgs/org.model';
import { CheckUser } from '../../../entities/orgs/org.actions';
import { checkUserStatus } from 'app/entities/orgs/org.selectors';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';

@Component({
  selector: 'app-migration-slider',
  templateUrl: './migration-slider.component.html',
  styleUrls: ['./migration-slider.component.scss']
})
export class MigrationSliderComponent implements OnChanges, OnDestroy {
  @Input() migrationID: string;
  @Input() isPreview: boolean;
  @Input() previewData;
  @Output() confirmPreview = new EventEmitter();
  @Output() cancelMigration = new EventEmitter();

  private isDestroyed = new Subject<boolean>();
  public checkedUser = false;
  public migrationForm: FormGroup;
  public usersData: User[];
  public selectedUsersData: User[] = [];
  public conflictedUsers: User[] = [];
  public notReadyToConfirm = true;
  public userExist: boolean;
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
      this.previewData.staged_data.users.forEach(
        (input_template: { automate_username: string | number, is_conflicting: boolean }) => {
        group[input_template.automate_username] = new FormControl(
          {value: '', disabled: !input_template.is_conflicting});
      });

      this.usersData = this.previewData.staged_data.users.map(
        (obj: User) => ({ ...obj, is_seleted: false })
      );
      this.conflictedUsers = this.usersData.filter((obj: User) => obj.is_conflicting);
      this.usersData.forEach((item: User) => item.is_selected = true);
      this.usersData.forEach((item: User) => this.selectedUsersData.push(item));
      this.checkNotreadyToConfirm();
    }
    this.migrationForm = new FormGroup(group);
  }

  ngOnDestroy(): void {
    this.isDestroyed.next();
    this.isDestroyed.complete();
  }

  closeMigrationSlider() {
    this.toggleSlide();
    this.selectedUsersData = [];
  }

  cancelMigrationclicked() {
    this.toggleSlide();
    this.cancelMigration.emit(this.migrationID);
    this.selectedUsersData = [];
  }

  toggleSlide() {
    this.isSlideOpen1 = !this.isSlideOpen1;
    this.usersData.forEach((item: User) => item.is_selected = false);
  }

  confirmPreviewData() {
    this.toggleSlide();
    this.confirmPreview.emit(this.selectedUsersData);
    this.selectedUsersData = [];
  }

  slidePanel() {
    this.isSlideOpen1 = true;
  }

  selectedAllUsers(event: any) {
    const checked = event.target.checked;
    this.usersData.forEach((item: User) => item.is_selected = checked);
    if (checked) {
      this.usersData.forEach((item: User) => this.selectedUsersData.push(item));
    } else {
      this.selectedUsersData = [];
    }
    this.checkNotreadyToConfirm();
  }

  selectedUser(event: Event, user: User) {
    const checkBox = document.getElementById(user.email + '-chef-checkbox');
    const input = document.getElementById(user.email + '-input');
    const warning = document.getElementById(user.email + '-warning');
    console.log(event);
    const index = this.usersData.findIndex(x => x.username === user.username);
    if (checkBox.textContent === 'check') {
      this.selectedUsersData.forEach((value, i) => {
        if (value.username === user.username) {
          this.selectedUsersData.splice(i, 1);
        }
      });
      if (user.is_conflicting) {
        input?.classList?.remove('user-exist-warning');
        warning?.classList?.add('warning-msg-hide');
      }
    } else {
      this.selectedUsersData.push(this.usersData[index]);
      warning?.classList?.remove('warning-msg-hide');
      this.callAndSetUserData(user);
    }
  }


  handleName(event: Event, user: User): void {
    this.callToCheckUsernameExist(event, user);
  }

  callToCheckUsernameExist(event: Event, user: User) {
    const checkBox = document.getElementById(user.email + '-chef-checkbox');
    console.log(event);

    if (checkBox.textContent === 'check') {
      this.callAndSetUserData(user);
    }
  }

  callAndSetUserData(user: User) {
    const input = document.getElementById(user.email + '-input');
    if ((input as HTMLInputElement).value !== '') {
      const payload = {
        user: (input as HTMLInputElement).value
      };
      this.store.dispatch(new CheckUser(payload));
      // check user status
      this.store.select(checkUserStatus).pipe(
        takeUntil(this.isDestroyed),
        filter(state => !pending (state)))
      .subscribe((state) => {
        this.userExist = (state === EntityStatus.loadingSuccess);
      });

      if (this.userExist) {
        input.classList.add('user-exist-warning');

        const index = this.usersData.findIndex(x => x.username === user.username);
        this.conflictedUsers.forEach((item: User) => {
          if (item.username !== user.username) {
            this.conflictedUsers.push(this.usersData[index]);
          }
        });

        // uniq data for the conflcted users
        this.conflictedUsers = [...new Set(this.conflictedUsers)];

        this.selectedUsersData.forEach((item: User) => {
          if (item.username === user.username) {
            item.is_conflicting = true;
          }
        });

        // uniq data for the selected users
        this.selectedUsersData = [...new Set(this.selectedUsersData)];

        this.usersData.forEach((item: User) => {
          if (item.username === user.username) {
            item.is_conflicting = true;
          }
        });

        // uniq data for the users data
        this.usersData = [...new Set(this.usersData)];

      } else {
        input.classList.remove('user-exist-warning');
        this.conflictedUsers.forEach((value, i) => {
          if (value.username === user.username) {
            this.conflictedUsers.splice(i, 1);
          }
        });

        // uniq data for the conflcted users
        this.conflictedUsers = [...new Set(this.conflictedUsers)];

        this.selectedUsersData.forEach((item: User) => {
          if (item.username === user.username) {
            item.is_conflicting = false;
          }
        });

        // uniq data for the selected users
        this.selectedUsersData = [...new Set(this.selectedUsersData)];

        this.usersData.forEach((item: User) => {
          if (item.username === user.username) {
            item.is_conflicting = false;
          }
        });

        // uniq data for the users data
        this.usersData = [...new Set(this.usersData)];
      }

      this.checkNotreadyToConfirm();
    }
  }

  checkNotreadyToConfirm() {
    const selectedConflictUsersCount =
        this.selectedUsersData.filter(e => e.is_conflicting === true).length;
      this.notReadyToConfirm = selectedConflictUsersCount === 0 ? false : true;
  }

  onChangeEvent(event: any) {
    const username = event.target.dataset.username;
    const index = this.usersData.findIndex(x => x.username === username);
    this.usersData[index].automate_username = event.target.value;
  }
}
