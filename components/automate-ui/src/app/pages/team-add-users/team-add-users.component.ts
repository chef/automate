import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { Router } from '@angular/router';
import { HttpErrorResponse } from '@angular/common/http';
import { combineLatest, Subject } from 'rxjs';
import { identity, keyBy, at } from 'lodash/fp';
import { filter, map, pluck, takeUntil } from 'rxjs/operators';
import { filter as lodashFilter } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams, routeURL } from 'app/route.selectors';
import { EntityStatus } from 'app/entities/entities';
import { User, HashMapOfUsers, userArrayToHash } from 'app/entities/users/user.model';
import { allUsers, userStatus } from 'app/entities/users/user.selectors';
import { GetUsers } from 'app/entities/users/user.actions';
import { iamMajorVersion } from 'app/entities/policies/policy.selectors';
import {
  v1TeamFromRoute,
  v2TeamFromRoute,
  teamUsers,
  addUsersStatus,
  getUsersStatus as getTeamUsersStatus,
  addUsersStatusError as addTeamUsersStatusError
} from 'app/entities/teams/team.selectors';
import { Team } from 'app/entities/teams/team.model';
import {
  GetTeam,
  GetTeamUsers,
  AddTeamUsers
} from 'app/entities/teams/team.actions';

const TEAM_ADD_USERS_ROUTE = /^\/settings\/teams\/.*\/add-users$/;

@Component({
  selector: 'app-team-add-users',
  templateUrl: './team-add-users.component.html',
  styleUrls: ['./team-add-users.component.scss']
})
export class TeamAddUsersComponent implements OnInit, OnDestroy {
  public team: Team;
  public users: User[] = [];
  private mapOfUsersToFilter: HashMapOfUsers = {};
  private isV1 = true;
  private isDestroyed = new Subject<boolean>();
  public addingUsers = false;
  private usersToAdd: { [id: string]: User } = {};
  public showSecondary = false;
  public addUsersFailed = '';
  public loading = true;

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router) {
  }

  ngOnInit(): void {
    this.store.select(iamMajorVersion)
      .pipe(takeUntil(this.isDestroyed))
      .subscribe((version) => {
        if (version === null) { return; }
        this.isV1 = version === 'v1';

        if (this.isV1) {
          this.store.select(v1TeamFromRoute)
            .pipe(filter(identity), takeUntil(this.isDestroyed))
            .subscribe(this.getUsersForTeam.bind(this));
        } else {
          this.store.select(v2TeamFromRoute)
            .pipe(filter(identity), takeUntil(this.isDestroyed))
            .subscribe(this.getUsersForTeam.bind(this));
        }
      });

    this.store.select(routeParams).pipe(
      pluck('id'),
      filter(identity),
      takeUntil(this.isDestroyed))
      .subscribe((id: string) => {
        this.store.select(routeURL).pipe(
          filter(identity),
          takeUntil(this.isDestroyed))
          .subscribe((url: string) => {
            // Only fetch if we are on the team details route, otherwise
            // we'll trigger GetTeam with the wrong input on any route
            // away to a page that also uses the :id param.
            if (TEAM_ADD_USERS_ROUTE.test(url)) {
              this.store.dispatch(new GetTeam({ id }));
            }
          });
      });

      // select all local users, the users for this team, and the query status for both.
      // when both queries are finished we'll find all users that are not yet a member of the team.
      combineLatest(
        this.store.select(allUsers),
        this.store.select(userStatus),
        this.store.select(teamUsers),
        this.store.select(getTeamUsersStatus))
        .pipe(
          map(([users, uStatus, tUsers, tStatus]) => {
            if (uStatus !== EntityStatus.loadingSuccess ||
              tStatus !== EntityStatus.loadingSuccess) {
              this.loading = true;
              return [];
            }
            const sortedUser = users.sort(
              (a, b) => {
                // See https://stackoverflow.com/a/38641281 for these options
                const opts = { numeric: true, sensitivity: 'base' };
                // sort by name then by id
                return a.name.localeCompare(b.name, undefined, opts) ||
                  a.name.localeCompare(b.name, undefined, { numeric: true }) ||
                  a.id.localeCompare(b.id, undefined, opts);
                });
            this.users = sortedUser;
            // Map UUID membership to user records and remove any entries that don't
            // map to user records.
            return at(tUsers, keyBy('membership_id', users))
              .filter(userRecord => userRecord !== undefined);
          }),
          map((users: User[]) => {
            this.mapOfUsersToFilter = userArrayToHash(users);
            this.loading = false;
          }),
          takeUntil(this.isDestroyed)
        ).subscribe();
 }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  addUsers(): void {
    this.addingUsers = true;
    this.addUsersFailed = '';
    const userIDs = Object.values(this.usersToAdd).map((user: User) => user.membership_id);
    this.store.dispatch(new AddTeamUsers({
      id: this.teamId,
      user_ids: userIDs
    }));

    const pendingAdd = new Subject<boolean>();
    this.store.select(addUsersStatus).pipe(
      filter(identity),
      takeUntil(pendingAdd))
      .subscribe((state) => {
        if (state === EntityStatus.loadingSuccess) {
          pendingAdd.next(true);
          pendingAdd.complete();
          this.addingUsers = false;
          this.closePage();
        }
        if (state === EntityStatus.loadingFailure) {
          this.store.select(addTeamUsersStatusError).pipe(
            filter(identity),
            takeUntil(pendingAdd)).subscribe((error: HttpErrorResponse) => {
              if (error.message === undefined) {
                this.addUsersFailed = 'An error occurred while attempting ' +
                'to add users. Please try again.';
              } else {
                this.addUsersFailed = `Failed to add users: ${error.message}`;
              }
              this.addingUsers = false;
          });
        }
      });
  }

  getHeading(): string {
    if (this.team) {
      return `Add Users to ${this.team.name}`;
    }
    return 'Loading...';
  }

  userNotFiltered(user: User): boolean {
    return !(user.membership_id in this.mapOfUsersToFilter);
  }

  usersNotFiltered(): User[] {
    return lodashFilter(user => this.userNotFiltered(user), this.users);
  }

  usersToAddValues(): User[] {
    return Object.values(this.usersToAdd);
  }

  addOrRemoveQueuedUser(checked: boolean, user: User): void {
    if (checked) {
      this.usersToAdd[user.id] = user;
    } else {
      delete this.usersToAdd[user.id];
    }
  }

  isUserChecked(user: User): boolean {
    return user.id in this.usersToAdd;
  }

  getConfirmBtnText(): string {
    if (this.addingUsers) {
      return (this.usersToAddValues().length < 2)
        ? 'Adding User...'
        : `Adding ${this.usersToAddValues().length} Users...`;
    } else {
      return (this.usersToAddValues().length < 2)
        ? 'Add User'
        : `Add ${this.usersToAddValues().length} Users`;
    }
  }

  private getUsersForTeam(team: Team): void {
    this.team = team;
    this.store.dispatch(new GetTeamUsers({ id: this.teamId }));
    this.store.dispatch(new GetUsers());
  }

  closePage(): void {
    this.router.navigate(['/settings', 'teams', this.teamId]);
  }

  getErrorMessage(): string {
    return this.addUsersFailed.length > 0 ? this.addUsersFailed : undefined;
  }

  private get teamId(): string {
    return this.isV1 ? this.team.guid : this.team.id;
  }
}
