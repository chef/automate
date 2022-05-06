import { Component, EventEmitter, HostBinding, Input, OnChanges, OnDestroy, Output } from '@angular/core';
import { FormBuilder, FormControl, FormGroup } from '@angular/forms';
import { combineLatest, Subject } from 'rxjs';
import { EntityStatus } from 'app/entities/entities';
import { takeUntil } from 'rxjs/operators';
import { User } from '../../../entities/orgs/org.model';
import { CheckUser } from '../../../entities/orgs/org.actions';
import { checkUserStatus, getCheckedUserStatus } from 'app/entities/orgs/org.selectors';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { isNil } from 'lodash';

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
  public totalUsers: User[];
  public usersData: User[];
  public selectedUsersData: User[] = [];
  public conflictedUsers: User[] = [];
  public skippedUsers: User[] = [];
  public deletedUsers: User[] = [];
  public updatedUsers: User[] = [];
  public userData: User;
  public notReadyToConfirm = true;
  public userExist: boolean;
  public checking_user = false;
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
      this.selectedUsersData = [];
      this.totalUsers = this.previewData.staged_data.users;
      // skipped users
      this.skippedUsers =
        this.previewData.staged_data.users.filter((obj: User) => obj.action_ops === 2);
      // deleted users
      this.deletedUsers =
        this.previewData.staged_data.users.filter((obj: User) => obj.action_ops === 3);
      // updated users
      this.updatedUsers =
        this.previewData.staged_data.users.filter((obj: User) => obj.action_ops === 4);
      // users for migration
      this.previewData.staged_data.users =
        this.previewData.staged_data.users.filter((obj: User) => obj.action_ops === 1);
      this.previewData.staged_data.users.forEach(
        (input_template: { automate_username: string | number, is_conflicting: boolean }) => {
        group[input_template.automate_username] = new FormControl(
          {value: '', disabled: !input_template.is_conflicting});
      });

      this.usersData = this.previewData.staged_data.users.map(
        (obj: User) => ({ ...obj, is_selected: false, checking_conflcit: false })
      );
      this.conflictedUsers = this.usersData.filter((obj: User) => obj.is_conflicting);
      this.usersData.forEach((item: User) => item.is_selected = true);
      this.usersData.forEach((item: User) => this.selectedUsersData.push(item));
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
    this.selectedUsersData = [];
  }

  confirmPreviewData() {
    const selectedUsersForMigation =
      this.selectedUsersData.filter(e => e.is_conflicting === true).length;
    if ( selectedUsersForMigation === 0 ) {
      this.confirmPreview.emit(this.selectedUsersData);
      this.toggleSlide();
    } else {
      this.selectedUsersData.forEach((item: User) => {
        if (item.is_conflicting) {
          const input = document.getElementById(item.email + '-input');
          const warning = document.getElementById(item.email + '-warning');
          input?.classList?.add('user-exist-warning');
          warning?.classList?.remove('warning-msg-hide');
        }
      });
      this.conflictedUsers.forEach((item: User) => {
        if (!item.is_selected) {
          const input = document.getElementById(item.email + '-input');
          input?.classList?.remove('user-exist-warning');
        }
      });
    }

  }

  slidePanel() {
    this.isSlideOpen1 = true;
  }

  selectedAllUsers(event: any) {
    const checked = event.target.checked;
    this.usersData.forEach((item: User) => item.is_selected = checked);
    if (checked) {
      this.usersData.forEach((item: User) => this.selectedUsersData.push(item));
      this.conflictedUsers = [];
      this.usersData.forEach((item: User) => {
        if (item.is_conflicting) {
          this.conflictedUsers.push(item);
        }
      });
    } else {
      this.selectedUsersData = [];
    }
  }

  selectedUser(event: Event, user: User) {
    const checkBox = document.getElementById(user.email + '-chef-checkbox');
    const input = document.getElementById(user.email + '-input');
    const warning = document.getElementById(user.email + '-warning');
    console.log(event.stopPropagation);
    const index = this.usersData.findIndex(x => x.username === user.username);
    if (checkBox.textContent === 'check') {
      this.selectedUsersData.forEach((value, i) => {
        if (value.username === user.username) {
          this.selectedUsersData.splice(i, 1);
        }
      });
      this.conflictedUsers.forEach((value, i) => {
        if (value.username === user.username) {
          this.conflictedUsers.splice(i, 1);
        }
      });
      if (user.is_conflicting) {
        input?.classList?.remove('user-exist-warning');
        warning?.classList?.add('warning-msg-hide');
      }
    } else {
      this.selectedUsersData.push(this.usersData[index]);
      this.selectedUsersData.forEach((item: User) => item.is_selected = true);
      warning?.classList?.remove('warning-msg-hide');
      this.callAndSetUserData(user);
    }
  }


  handleName(event: Event, user: User): void {
    this.checking_user = true;
    user.checking_conflcit = true;
    this.callToCheckUsernameExist(event, user);
  }

  callToCheckUsernameExist(event: Event, user: User) {
    const checkBox = document.getElementById(user.email + '-chef-checkbox');
    console.log(event.stopPropagation);

    if (checkBox.textContent === 'check') {
      setTimeout(() => { this.callAndSetUserData(user); }, 2000);
    }
  }

  callAndSetUserData(user: User) {
    this.userData = user;
    const input = document.getElementById(this.userData.email + '-input');
    const username = this.userData.username;
    const index = this.usersData.findIndex(x => x.username === username);

    // console.log(automate_username);
    if ((input as HTMLInputElement).value !== '') {
      const payload = {
        user: (input as HTMLInputElement).value
      };
      this.store.dispatch(new CheckUser(payload));
      // check user status

      combineLatest([
        this.store.select(checkUserStatus),
        this.store.select(getCheckedUserStatus)
      ]).pipe(takeUntil(this.isDestroyed))
        .subscribe(([checkUserSt, getCheckedUserState]) => {
          if (checkUserSt === EntityStatus.loadingSuccess && !isNil(getCheckedUserState)) {
            if ((input as HTMLInputElement).value === getCheckedUserState.user.id) {
              input.classList.add('user-exist-warning');
              const index1 = this.usersData.findIndex(x => x.username === user.username);
              this.conflictedUsers.forEach((item: User) => {
                if (item.username !== user.username) {
                  this.conflictedUsers.push(this.usersData[index1]);
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

              // reassign the old username
              this.usersData[index].automate_username =
                (input as HTMLInputElement).dataset.username;
            }
          } else {
            if (this.userData.automate_username === username) {
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

              // set the changed username
              this.usersData[index].automate_username = (input as HTMLInputElement).value;
            }
          }
        });
    }
    user.checking_conflcit = false;
  }
}
