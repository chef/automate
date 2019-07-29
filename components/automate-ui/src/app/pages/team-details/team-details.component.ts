import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Store, select } from '@ngrx/store';
import { identity, keyBy, at } from 'lodash/fp';
import { combineLatest, Observable, Subject } from 'rxjs';
import { filter, map, pluck, takeUntil } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams, routeURL } from 'app/route.selectors';
import { EntityStatus, loading } from 'app/entities/entities';
import { User, HashMapOfUsers } from 'app/entities/users/user.model';
import { allUsers, userStatus } from 'app/entities/users/user.selectors';
import { GetUsers } from 'app/entities/users/user.actions';
import { iamMajorVersion } from 'app/entities/policies/policy.selectors';
import {
  v1TeamFromRoute,
  v2TeamFromRoute,
  teamUsers,
  getStatus,
  getUsersStatus,
  updateStatus
} from 'app/entities/teams/team.selectors';
import { Team } from 'app/entities/teams/team.model';
import {
  AddTeamUsers,
  GetTeam,
  GetTeamUsers,
  TeamUserMgmtPayload,
  RemoveTeamUsers,
  UpdateTeam
} from 'app/entities/teams/team.actions';
import { Regex } from 'app/helpers/auth/regex';

const TEAM_DETAILS_ROUTE = /^\/settings\/teams/;

export type TeamTabName = 'users' | 'details';

@Component({
  selector: 'app-team-details',
  templateUrl: './team-details.component.html',
  styleUrls: ['./team-details.component.scss']
})
export class TeamDetailsComponent implements OnInit, OnDestroy {
  public updateNameForm: FormGroup;
  // isLoading represents the initial load as well as subsequent updates in progress.
  public isLoading = true;
  public saving = false;
  public saveSuccessful = false;
  public tabValue: TeamTabName = 'users';
  public url: string;
  public teamMembershipView = false;
  public team: Team;
  public isV1 = true;

  public sortedUsers$: Observable<User[]>;
  private isDestroyed = new Subject<boolean>();

  public addButtonText = 'Add User';
  public removeText = 'Remove User';

  constructor(private store: Store<NgrxStateAtom>,
    public fb: FormBuilder,
    private router: Router
  ) {
    this.updateNameForm = fb.group({
      // Must stay in sync with error checks in team-details.component.html
      name: ['Loading...', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });

    combineLatest(
      this.store.select(getStatus),
      this.store.select(updateStatus)
    ).pipe(
      takeUntil(this.isDestroyed),
      map(([gStatus, uStatus]) => {
        this.isLoading =
          (gStatus !== EntityStatus.loadingSuccess) ||
          (uStatus === EntityStatus.loading);
      })
    ).subscribe();
  }

  private get teamId(): string {
    return this.isV1 ? this.team.guid : this.team.id;
  }

  public get descriptionOrName(): string {
    return this.isV1 ? 'description' : 'name';
  }

  ngOnInit(): void {
    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
      .subscribe((url: string) => {
        this.url = url;
        const [, fragment] = url.split('#');
        // goes to #users if (1) explicit #users, (2) no fragment, or (3) invalid fragment
        this.tabValue = (fragment === 'details') ? 'details' : 'users';
      });
    this.store.select(iamMajorVersion)
      .pipe(takeUntil(this.isDestroyed))
      .subscribe((version) => {
        if (version === null) { return; }
        this.isV1 = version === 'v1';

        // Triggered every time the team is updated.
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

    this.sortedUsers$ = <Observable<User[]>>combineLatest(
      this.store.select(allUsers),
      this.store.select(userStatus),
      this.store.select(teamUsers),
      this.store.select(getUsersStatus))
      .pipe(
        map(([users, uStatus, tUsers, tStatus]) => {
          if (uStatus !== EntityStatus.loadingSuccess ||
            tStatus !== EntityStatus.loadingSuccess) {
            return [];
          }
          // Map UUID membership to user records and remove any entries that don't
          // map to user records.
          return at(tUsers, keyBy('membership_id', users))
            .filter(userRecord => userRecord !== undefined);
        }),
        map((users: User[]) => users.sort(
          (a, b) => {
            // See https://stackoverflow.com/a/38641281 for these options
            const opts = { numeric: true, sensitivity: 'base' };
            // sort by name then by id
            return a.name.localeCompare(b.name, undefined, opts) ||
              a.name.localeCompare(b.name, undefined, { numeric: true }) ||
              a.id.localeCompare(b.id, undefined, opts);
            })),
        takeUntil(this.isDestroyed)
      );

    // If, however, the user browses directly to /settings/teams/ID, the store
    // will not contain the team data, so we fetch it.
    // TODO: This also fires when teamFromRoute fires; should inhibit that since
    // we already have the team details, as noted above.
    // Triggered every time the route changes (including on initial load).
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
            if (TEAM_DETAILS_ROUTE.test(url)) {
              this.store.dispatch(new GetTeam({ id }));
            }
          });
      });
 }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  private getUsersForTeam(team: Team) {
    this.team = team;
    this.updateNameForm.controls.name.setValue(this.team.name);
    this.store.dispatch(new GetTeamUsers({ id: this.teamId }));
    this.store.dispatch(new GetUsers());
  }

  toggleUserMembershipView(): void {
    this.teamMembershipView = !this.teamMembershipView;
  }

  addUsers(users: HashMapOfUsers): void {
    const userIDs = Object.values(users).map((user: User) => user.membership_id);

    this.store.dispatch(new AddTeamUsers(<TeamUserMgmtPayload>{
      id: this.teamId,
      user_ids: userIDs
    }));
    this.toggleUserMembershipView();
  }

  removeUser(user: User): void {
    this.store.dispatch(new RemoveTeamUsers(<TeamUserMgmtPayload>{
      id: this.teamId,
      user_ids: [user.membership_id]
    }));
  }

  public keyPressed() {
    this.saveSuccessful = false;
  }

  public saveNameChange(): void {
    this.saveSuccessful = false;
    this.saving = true;
    const name: string = this.updateNameForm.controls.name.value.trim();
    this.store.dispatch(new UpdateTeam({ ...this.team, name }));

    const pendingSave = new Subject<boolean>();
    this.store.pipe(
      select(updateStatus),
      filter(identity),
      takeUntil(pendingSave))
      .subscribe((state) => {
        if (!loading(state)) {
          pendingSave.next(true);
          pendingSave.complete();
          this.saving = false;
          this.saveSuccessful = (state === EntityStatus.loadingSuccess);
        }
      });
  }

  onSelectedTab(event): void {
    this.tabValue = event.target.value;
    // Current URL sans any now outdated fragment.
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }
}
