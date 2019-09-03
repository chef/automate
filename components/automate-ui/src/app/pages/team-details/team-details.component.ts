import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, FormControl, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Store, select } from '@ngrx/store';
import { isEmpty, identity, keyBy, at, xor } from 'lodash/fp';
import { combineLatest, Observable, Subject } from 'rxjs';
import { filter, map, pluck, takeUntil } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams, routeURL } from 'app/route.selectors';
import { EntityStatus, loading } from 'app/entities/entities';
import { User } from 'app/entities/users/user.model';
import { Regex } from 'app/helpers/auth/regex';
import { allUsers, userStatus } from 'app/entities/users/user.selectors';
import { GetUsers } from 'app/entities/users/user.actions';
import {
  iamMajorVersion,
  iamMinorVersion,
  atLeastV2p1
} from 'app/entities/policies/policy.selectors';
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
  GetTeam,
  GetTeamUsers,
  TeamUserMgmtPayload,
  RemoveTeamUsers,
  UpdateTeam
} from 'app/entities/teams/team.actions';
import {
  ProjectChecked,
  ProjectCheckedMap
} from 'app/components/projects-dropdown/projects-dropdown.component';

import { GetProjects } from 'app/entities/projects/project.actions';
import {
  allProjects,
  getAllStatus as getAllProjectStatus
} from 'app/entities/projects/project.selectors';
import { ProjectConstants, Project } from 'app/entities/projects/project.model';
import { IAMMajorVersion } from 'app/entities/policies/policy.model';

const TEAM_DETAILS_ROUTE = /^\/settings\/teams/;

export type TeamTabName = 'users' | 'details';

@Component({
  selector: 'app-team-details',
  templateUrl: './team-details.component.html',
  styleUrls: ['./team-details.component.scss']
})
export class TeamDetailsComponent implements OnInit, OnDestroy {
  public updateNameForm: FormGroup;
  // isLoadingTeam represents the initial team load as well as subsequent updates in progress.
  public isLoadingTeam = true;
  public saving = false;
  public saveSuccessful = false;
  public tabValue: TeamTabName = 'users';
  private url: string;
  public teamMembershipView = false;

  public team: Team;
  public isMajorV1 = true;
  public isMinorV1 = false;

  public sortedUsers$: Observable<User[]>;
  private isDestroyed = new Subject<boolean>();

  public addButtonText = 'Add Users';
  public removeText = 'Remove User';

  public atLeastV2p1$: Observable<boolean>;
  public projectsEnabled: boolean;
  public projects: ProjectCheckedMap = {};
  public unassigned = ProjectConstants.UNASSIGNED_PROJECT_ID;

  constructor(private store: Store<NgrxStateAtom>,
    public fb: FormBuilder,
    private router: Router
  ) {
    this.team = {
      id: '',
      name: '',
      guid: null,
      projects: []
    };
    this.updateNameForm = fb.group({
      // Must stay in sync with error checks in team-details.component.html.
      // Also, initialize the form to disabled and enable after team load
      // to prevent people from typing before the team is fetched and have their
      // value overwritten.
      name: new FormControl({value: 'Loading...', disabled: true},
        [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]),
      projects: [[]]
    });
    this.store.pipe(
      select(atLeastV2p1),
      takeUntil(this.isDestroyed))
      .subscribe(projectsEnabled => {
        this.projectsEnabled = projectsEnabled;
      });
  }

  private get teamId(): string {
    return this.isMajorV1 ? this.team.guid : this.team.id;
  }

  public get descriptionOrName(): string {
    return this.isMajorV1 ? 'description' : 'name';
  }

  ngOnInit(): void {
    this.atLeastV2p1$ = this.store.select(atLeastV2p1);

    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
      .subscribe((url: string) => {
        this.url = url;
        const [, fragment] = url.split('#');
        // goes to #users if (1) explicit #users, (2) no fragment, or (3) invalid fragment
        this.tabValue = (fragment === 'details') ? 'details' : 'users';
      });

    this.store.select(iamMinorVersion)
      .pipe(takeUntil(this.isDestroyed))
      .subscribe((minor) => {
        this.isMinorV1 = minor === 'v1';
      });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(updateStatus)
    ]).pipe(
      takeUntil(this.isDestroyed),
      map(([gStatus, uStatus]) => {
        this.isLoadingTeam =
          (gStatus !== EntityStatus.loadingSuccess) ||
          (uStatus === EntityStatus.loading);
        if (this.isLoadingTeam) {
          this.updateNameForm.controls['name'].disable();
        } else {
          this.updateNameForm.controls['name'].enable();
        }
      })
    ).subscribe();

    combineLatest([
      this.store.select(v1TeamFromRoute),
      this.store.select(v2TeamFromRoute),
      this.store.select(iamMajorVersion)
    ]).pipe(
      takeUntil(this.isDestroyed),
      map(([v1Team, v2Team, major]: [Team, Team, IAMMajorVersion]) => {
        this.isMajorV1 = major === 'v1';
        return (this.isMajorV1 ? v1Team : v2Team);
      }),
      filter(identity)
    ).subscribe((team: Team) => {
      this.team = team;
      this.updateNameForm.controls.name.setValue(this.team.name);
      this.store.dispatch(new GetTeamUsers({ id: this.teamId }));
      this.store.dispatch(new GetUsers());
      if (this.projectsEnabled) {
        this.store.dispatch(new GetProjects());
      }
    });

    combineLatest([
      this.store.select(allProjects),
      this.store.select(getAllProjectStatus)
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(([_, pStatus]: [Project[], EntityStatus]) => pStatus !== EntityStatus.loading),
      filter(() => !!this.team),
      map(([allowedProjects, _]) => {
        this.projects = {};
        allowedProjects
          .forEach(p => {
            this.projects[p.id] = { ...p, checked: this.team.projects.includes(p.id)
            };
          });
      }))
      .subscribe();

    this.sortedUsers$ = <Observable<User[]>>combineLatest([
      this.store.select(allUsers),
      this.store.select(userStatus),
      this.store.select(teamUsers),
      this.store.select(getUsersStatus)])
      .pipe(
        map(([users, uStatus, tUsers, tStatus]: [User[], EntityStatus, string[], EntityStatus]) => {
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

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  toggleUserMembershipView(): void {
    this.router.navigate(['/settings', 'teams', this.teamId, 'add-users']);
  }

  removeUser(user: User): void {
    this.store.dispatch(new RemoveTeamUsers(<TeamUserMgmtPayload>{
      id: this.teamId,
      user_ids: [user.membership_id]
    }));
  }

  saveTeam(): void {
    this.saveSuccessful = false;
    this.saving = true;
    this.updateNameForm.controls['name'].disable();
    const name: string = this.updateNameForm.controls.name.value.trim();
    const projects = Object.keys(this.projects).filter(id => this.projects[id].checked);
    this.store.dispatch(new UpdateTeam({ ...this.team, name, projects }));

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
          this.updateNameForm.controls['name'].enable();
          this.saveSuccessful = (state === EntityStatus.loadingSuccess);
          if (this.saveSuccessful) {
            this.updateNameForm.markAsPristine();
          }
        }
      });
  }

  onSelectedTab(event: { target: { value: TeamTabName } }): void {
    this.tabValue = event.target.value;
    // Drop the previous fragment and add the incoming fragment.
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }

  // updates whether the project was checked or unchecked
  onProjectChecked(project: ProjectChecked): void {
    this.projects[project.id].checked = project.checked;

    // since the app-projects-dropdown is not a true form input (select)
    // we have to manage the form reactions
    if (this.noProjectsUpdated()) {
      this.updateNameForm.controls.projects.markAsPristine();
    } else {
      this.updateNameForm.controls.projects.markAsDirty();
    }

  }

  private noProjectsUpdated(): boolean {
    const projectsUpdated = xor(
      this.team.projects,
      Object.keys(this.projects).filter(id => this.projects[id].checked));
    return projectsUpdated.length === 0;
  }

  dropdownDisabled(): boolean {
    return isEmpty(this.projects) || this.saving;
  }
}
