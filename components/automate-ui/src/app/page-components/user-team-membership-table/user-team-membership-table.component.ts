import { takeUntil, map } from 'rxjs/operators';
import { Component, OnInit, OnDestroy, Input, Output, EventEmitter } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable ,  Subject } from 'rxjs';
import { filter } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { EntityStatus } from 'app/entities/entities';
import { allUsers, userStatus } from 'app/entities/users/user.selectors';
import { GetUsers } from 'app/entities/users/user.actions';
import { User, HashMapOfUsers, userArrayToHash } from 'app/entities/users/user.model';

@Component({
  selector: 'app-user-team-membership-table',
  templateUrl: './user-team-membership-table.component.html',
  styleUrls: ['./user-team-membership-table.component.scss']
})
export class UserTeamMembershipTableComponent implements OnInit, OnDestroy {
  // Observable array of users that have already been added so we can
  // filter them out of the table of users to add.
  @Input() usersToFilter: Observable<User[]>;

  @Output() addClicked = new EventEmitter<HashMapOfUsers>();
  @Output() cancelClicked = new EventEmitter();

  // List of possible users to add
  public users: User[] = [];
  public mapOfUsersToFilter: HashMapOfUsers = {};
  public userStatus$: Observable<EntityStatus>;
  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  public usersToAdd: HashMapOfUsers = {};
  public usersToFilterQueried = false;

  constructor(private store: Store<NgrxStateAtom>) {
    this.userStatus$ = store.select(userStatus);
    store.select(allUsers).pipe(
      map((users: User[]) => users.sort(
        (a, b) => {
          // See https://stackoverflow.com/a/38641281 for these options
          const opts = { numeric: true, sensitivity: 'base' };
          // sort by name then by username
          return a.name.localeCompare(b.name, undefined, opts) ||
            a.name.localeCompare(b.name, undefined, { numeric: true}) ||
            a.id.localeCompare(b.id, undefined, opts);
        }
      )),
      takeUntil(this.isDestroyed))
      .subscribe(this.handleUsers.bind(this));
  }

  ngOnInit() {
    this.store.dispatch(new GetUsers());
    this.subscribeToUsersToFilter();
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  private handleUsers(users: User[]): void {
    this.users = users;
  }

  addOrRemoveUser(checked: boolean, user: User): void {
    if (checked) {
      this.usersToAdd[user.id] = user;
    } else {
      delete this.usersToAdd[user.id];
    }
  }

  public loading(status: EntityStatus): boolean {
    return status === EntityStatus.loading || !this.usersToFilterQueried;
  }

  addEvent(): void {
    this.addClicked.emit(this.usersToAdd);
  }

  public userNotFiltered(user: User): boolean {
    return !(user.id in this.mapOfUsersToFilter);
  }

  public usersNotFiltered(): User[] {
    return filter(user => this.userNotFiltered(user), this.users);
  }

  public noUsersAvailable(): boolean {
    return this.usersNotFiltered().length === 0;
  }

  private noUsersAdded(): boolean {
    return Object.keys(this.usersToAdd).length === 0;
  }

  public noUsersAddedOrAvailable(): boolean {
    return this.noUsersAvailable() || this.noUsersAdded();
  }

  public userLink(user: User): string {
    return `/settings/users/${user.id}`;
  }

  public subscribeToUsersToFilter(): void {
    this.usersToFilter.subscribe((users: User[]) => {
      this.mapOfUsersToFilter = userArrayToHash(users);
      this.usersToFilterQueried = true;
    });
  }
}
