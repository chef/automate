import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Store } from '@ngrx/store';
import { identity, keyBy, at } from 'lodash/fp';
import { combineLatest, Observable, Subject } from 'rxjs';
import { filter, map, pluck, takeUntil } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams } from 'app/route.selectors';
import { EntityStatus } from 'app/entities/entities';
import { User, HashMapOfUsers } from 'app/entities/users/user.model';
import { allUsers, userStatus } from 'app/entities/users/user.selectors';
import { GetUsers } from 'app/entities/users/user.actions';
import { iamMajorVersion } from 'app/entities/policies/policy.selectors';
import {
  v1TeamFromRoute,
  v2TeamFromRoute,
  teamUsers,
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

// NB: neither \S nor ^\s work inside the brackets in this regex language.
const NON_BLANK = '.*[^ ].*';

@Component({
  selector: 'app-team-details',
  templateUrl: './team-details.component.html',
  styleUrls: ['./team-details.component.scss']
})
export class TeamDetailsComponent implements OnInit, OnDestroy {

  public teamMembershipView = false;
  public editMode = false;
  public team: Team;
  public editForm: FormGroup;
  private isV1 = true;

  public sortedUsers$: Observable<User[]>;
  private isDestroyed = new Subject<boolean>();

  public addButtonText = 'Add User';
  public removeText = 'Remove User';

  constructor(private store: Store<NgrxStateAtom>,
    private fb: FormBuilder
  ) {
    this.createForms(this.fb);
  }

  private get teamId(): string {
    return this.isV1 ? this.team.guid : this.team.id;
  }

  public get versionedDescOrName(): string {
    return this.isV1 ? 'description' : 'name';
  }

  ngOnInit(): void {
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
          return at(tUsers, keyBy('id', users))
            .filter(userRecord => userRecord !== undefined);
        }),
        map((users: User[]) => users.sort(
          (a, b) => {
            // See https://stackoverflow.com/a/38641281 for these options
            const opts = { numeric: true, sensitivity: 'base' };
            // sort by name then by username
            return a.name.localeCompare(b.name, undefined, opts) ||
              a.name.localeCompare(b.name, undefined, { numeric: true }) ||
              a.username.localeCompare(b.username, undefined, opts);
          }))
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
        this.store.dispatch(new GetTeam({ id }));
      });

    this.store.select(updateStatus).pipe(
      takeUntil(this.isDestroyed))
      .subscribe((state: EntityStatus) => {
        this.handleSave(state);
      });
 }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  private createForms(fb: FormBuilder): void {
    this.editForm = fb.group({
      teamName: ['', [Validators.required, Validators.pattern(NON_BLANK)]]
    });
  }

  private getUsersForTeam(team: Team) {
    this.team = team;
    this.store.dispatch(new GetTeamUsers({ id: this.teamId }));
    this.store.dispatch(new GetUsers());
  }

  toggleUserMembershipView(): void {
    this.teamMembershipView = !this.teamMembershipView;
  }

  addUsers(users: HashMapOfUsers): void {
    this.toggleUserMembershipView();

    const userIDs = Object.values(users).map((user: User) => user.id);

    this.store.dispatch(new AddTeamUsers(<TeamUserMgmtPayload>{
      id: this.teamId,
      user_ids: userIDs
    }));
  }

  removeUser(user: User): void {
    this.store.dispatch(new RemoveTeamUsers(<TeamUserMgmtPayload>{
      id: this.teamId,
      user_ids: [user.id]
    }));
  }

  updateTeam(): void {
    const newTeam = <Team>Object.assign(
      {}, this.team, { name: this.editForm.get('teamName').value });
    this.store.dispatch(new UpdateTeam(newTeam));
  }

  setEditMode(isEditMode: boolean): void {
    this.editForm.patchValue(
      {
        teamName: this.team.name
      });
    this.editMode = isEditMode;
  }

  handleSave(state: EntityStatus): void {
    if (state === EntityStatus.loadingSuccess) {
      this.editMode = false;
    }
  }
}
