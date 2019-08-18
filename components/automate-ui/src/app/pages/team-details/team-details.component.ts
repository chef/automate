import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Store, select } from '@ngrx/store';
import { identity, keyBy, at, xor } from 'lodash/fp';
import { combineLatest, Observable, Subject } from 'rxjs';
import { filter, map, pluck, takeUntil } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams, routeURL } from 'app/route.selectors';
import { EntityStatus, loading } from 'app/entities/entities';
import { User } from 'app/entities/users/user.model';
import { Regex } from 'app/helpers/auth/regex';
import { allUsers, userStatus } from 'app/entities/users/user.selectors';
import { GetUsers } from 'app/entities/users/user.actions';
import { iamMajorVersion, iamMinorVersion } from 'app/entities/policies/policy.selectors';
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
import { GetProject } from 'app/entities/projects/project.actions';
import {
  getStatus as getProjectStatus,
  projectEntities
} from 'app/entities/projects/project.selectors';
import {
  ProjectChecked,
  ProjectCheckedMap
} from 'app/components/projects-dropdown/projects-dropdown.component';

import { ProjectConstants, Project } from 'app/entities/projects/project.model';
import { assignableProjects } from 'app/services/projects-filter/projects-filter.selectors';
import { ProjectsFilterOption } from 'app/services/projects-filter/projects-filter.reducer';
import { IAMType } from 'app/entities/policies/policy.model';

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

  public unassigned = ProjectConstants.UNASSIGNED_PROJECT_ID;
  public projects: ProjectCheckedMap = {};
  // keep a list of projects that were in the teams list that are
  // left to fetch so we can know if we are fully loaded or not.
  // don't want to enable the projects-dropdown until we have
  // checked the proper project checkboxes corresponding to projects
  // already in the team.
  public teamProjectsLeftToFetch: string[] = [];

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
      // Must stay in sync with error checks in team-details.component.html
      name: ['Loading...', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
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
      })
    ).subscribe();
  }

  private get teamId(): string {
    return this.isMajorV1 ? this.team.guid : this.team.id;
  }

  public get descriptionOrName(): string {
    return this.isMajorV1 ? 'description' : 'name';
  }

  ngOnInit(): void {
    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
      .subscribe((url: string) => {
        this.url = url;
        const [, fragment] = url.split('#');
        // goes to #users if (1) explicit #users, (2) no fragment, or (3) invalid fragment
        this.tabValue = (fragment === 'details') ? 'details' : 'users';
      });

    this.store.select(iamMinorVersion)
      .pipe(
        takeUntil(this.isDestroyed),
        filter(identity)
      )
      .subscribe((version) => {
        this.isMinorV1 = version === 'v1';
      });

    this.store.select(assignableProjects)
      .subscribe((assignable: ProjectsFilterOption[]) => {
        assignable.forEach(({ value: id, label: name, type: stringType }) => {
          const type = <IAMType>stringType;
          // TODO (tc) Don't actually have the status queried in here but
          // also don't need it for this page. Maybe the status field
          // should be optional in the project model?
          const proj: Project = { id, name, type, status: 'NO_RULES' };
          // we don't want to override projects that we fetched
          // that were part of the team already
          if (!this.projects[proj.id]) {
            const checked = false;
            this.projects[proj.id] = { ...proj, checked };
          }
        });
      });

    this.store.select(iamMajorVersion)
      .pipe(
        takeUntil(this.isDestroyed),
        filter(identity)
        )
      .subscribe((version) => {
        this.isMajorV1 = version === 'v1';

        // Triggered every time the team is updated.
        if (this.isMajorV1) {
          this.store.select(v1TeamFromRoute)
            .pipe(filter(identity), takeUntil(this.isDestroyed))
            .subscribe(this.getTeamDependentData.bind(this));
        } else {
          this.store.select(v2TeamFromRoute)
            .pipe(filter(identity), takeUntil(this.isDestroyed))
            .subscribe(this.getTeamDependentData.bind(this));
        }
      });

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

    // we keep a list of all projects that were left in the team
    // to know if we are fully loaded yet or not.
    combineLatest([
      this.store.select(getProjectStatus),
      this.store.select(projectEntities)])
      .pipe(filter(([status, _]: [EntityStatus, ProjectCheckedMap]) =>
          status === EntityStatus.loadingSuccess),
        map(([_, projectMap]) => {
          const projectsFound: { [id: string]: boolean } = {};
          this.teamProjectsLeftToFetch.forEach(pID => {
            const project = projectMap[pID];
            if (project !== undefined) {
              const checked = true;
              this.projects[project.id] = { ...project, checked };
              projectsFound[pID] = true;
            }
          });

          // Remove all team projects that have been fetched.
          this.teamProjectsLeftToFetch =
            this.teamProjectsLeftToFetch.filter(pID => !projectsFound[pID]);
      })).subscribe();
 }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  private getTeamDependentData(team: Team): void {
    this.team = team;
    this.team.projects.forEach(pID => {
      this.teamProjectsLeftToFetch.push(pID);
      this.store.dispatch(new GetProject({ id: pID }));
    });
    this.updateNameForm.controls.name.setValue(this.team.name);
    this.store.dispatch(new GetTeamUsers({ id: this.teamId }));
    this.store.dispatch(new GetUsers());
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

  public saveTeam(): void {
    this.saveSuccessful = false;
    this.saving = true;
    const name: string = this.updateNameForm.controls.name.value.trim();
    this.store.dispatch(new UpdateTeam({
        id: this.team.id,
        name: name,
        guid: this.team.guid, // to be deprecated after GA
        projects: Object.values(this.projects).filter(p => p.checked).map(p => p.id)
      }));

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
  }

  noProjectsUpdated(): boolean {
    return xor(this.team.projects,
      Object.values(this.projects).filter(p => p.checked).map(p => p.id)).length === 0;
  }

  dropdownDisabled(): boolean {
    return Object.values(this.projects).length === 0 || this.teamProjectsLeftToFetch.length !== 0;
  }
}
