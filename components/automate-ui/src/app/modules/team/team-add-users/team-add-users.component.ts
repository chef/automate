import { Component, OnInit, OnDestroy, EventEmitter } from '@angular/core';
import { Store, select } from '@ngrx/store';
import { Router } from '@angular/router';
import { combineLatest, Subject, Observable } from 'rxjs';
import { keyBy, at, identity } from 'lodash/fp';
import { filter, map, takeUntil, distinctUntilChanged } from 'rxjs/operators';
import { filter as lodashFilter } from 'lodash/fp';

import { ChefSorters } from 'app/helpers/auth/sorter';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeState } from 'app/route.selectors';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { EntityStatus, allLoadedSuccessfully, pending } from 'app/entities/entities';
import { User, HashMapOfUsers, userArrayToHash } from 'app/entities/users/user.model';
import { allUsers, getStatus as getAllUsersStatus } from 'app/entities/users/user.selectors';
import { GetUsers } from 'app/entities/users/user.actions';
import {
  teamFromRoute,
  teamUsers,
  addUsersStatus,
  getUsersStatus as getTeamUsersStatus
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
  private isDestroyed = new Subject<boolean>();
  public addingUsers = false;
  private usersToAdd: { [id: string]: User } = {};
  public showSecondary = false;
  public addUsersFailed = '';
  public loading$: Observable<boolean>;
  public teamId = '';
  public openUserModal = new EventEmitter<boolean>();

  constructor(
    private layoutFacade: LayoutFacadeService,
    private store: Store<NgrxStateAtom>,
    private router: Router) {
  }

  private fired = false;

  ngOnInit(): void {
    this.store.dispatch(new GetUsers());
    this.store.select(teamFromRoute).pipe(
      takeUntil(this.isDestroyed),
      filter(identity),
      map(team => [team, team ? team.id : null]),
      distinctUntilChanged(
        ([_teamA, teamIdA]: [Team, string], [_teamB, teamIdB]: [Team, string]) =>
        teamIdA === teamIdB)
    ).subscribe(([team, teamId]) => {
      this.team = team;
      this.teamId = teamId;
      this.store.dispatch(new GetTeamUsers({ id: teamId}));
    });

    this.store.select(routeState).pipe(
      takeUntil(this.isDestroyed),
      map(state => [state.params.id as string, state.url]),
      // Only fetch if we are on the team details route, otherwise
      // we'll trigger GetTeam with the wrong input on any route
      // away to a page that also uses the :id param.
      filter(([id, url]) => TEAM_ADD_USERS_ROUTE.test(url) && id !== undefined),
      map(([id, _url]) => id),
      distinctUntilChanged()
    ).subscribe(id => this.store.dispatch(new GetTeam({ id })));

    this.loading$ = combineLatest([
      this.store.select(getAllUsersStatus),
      this.store.select(getTeamUsersStatus)]).pipe(
        map((statuses: EntityStatus[]) => !allLoadedSuccessfully(statuses)));

    // select all local users, the users for this team, and the query status for both.
    // when both queries are finished we'll find all users that are not yet a member of the team.
    combineLatest([
      this.loading$,
      this.store.select(allUsers),
      this.store.select(teamUsers)])
    .pipe(
      takeUntil(this.isDestroyed),
      filter(([loading, _users, _tUsers]) => !loading)
    ).subscribe(([_loading, users, tUsers]) => {
      // Sort users naturally first by name, then by id
      this.users = ChefSorters.naturalSort(users, ['name', 'id']);

      const membershipUsers = at(tUsers, keyBy('membership_id', users))
        .filter(userRecord => userRecord !== undefined);

      this.mapOfUsersToFilter = userArrayToHash(membershipUsers);
    });

    this.store.pipe(
      select(addUsersStatus),
      takeUntil(this.isDestroyed),
      filter(state => this.addingUsers && !pending(state)))
      .subscribe(() => {
          this.addingUsers = false;
          this.closePage();
    });
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
      membership_ids: userIDs
    }));
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
    if (this.usersNotFiltered().length === 0) {
      return '';
    } else if (this.addingUsers) {
      return (this.usersToAddValues().length < 2)
        ? 'Adding User...'
        : `Adding ${this.usersToAddValues().length} Users...`;
    } else {
      return (this.usersToAddValues().length < 2)
        ? 'Add User'
        : `Add ${this.usersToAddValues().length} Users`;
    }
  }

  closePage(): void {
    this.router.navigate(['/settings', 'teams', this.teamId]);
  }

  getErrorMessage(): string {
    return this.addUsersFailed.length > 0 ? this.addUsersFailed : undefined;
  }

  openModal(): void {
    this.openUserModal.emit(true);
  }

  triggerFullPageMessage() {
    // `fired` variable just minimizes browser's workload since that gets called continuously
    if (!this.fired) {
      this.layoutFacade.showFullPage();
      this.fired = true;
    }
  }
}
