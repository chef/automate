import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, FormControl, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Store, select } from '@ngrx/store';
import { isEmpty, keyBy, at, xor, isNil } from 'lodash/fp';
import { combineLatest, Subject, Observable } from 'rxjs';
import { filter, map, takeUntil, distinctUntilChanged } from 'rxjs/operators';

import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { ChefSorters } from 'app/helpers/auth/sorter';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeURL, routeState } from 'app/route.selectors';
import { EntityStatus, pending } from 'app/entities/entities';
import { User } from 'app/entities/users/user.model';
import { Regex } from 'app/helpers/auth/regex';
import { allUsers, getStatus as getAllUsersStatus } from 'app/entities/users/user.selectors';
import { GetUsers } from 'app/entities/users/user.actions';
import { isIAMv2 } from 'app/entities/policies/policy.selectors';
import {
  v1TeamFromRoute,
  v2TeamFromRoute,
  teamUsers,
  getStatus,
  getUsersStatus as getTeamUsersStatus,
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
import { ProjectConstants } from 'app/entities/projects/project.model';

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
  public saveInProgress = false;
  public saveSuccessful = false;
  public tabValue: TeamTabName = 'users';
  private url: string;
  public teamMembershipView = false;

  public team: Team;
  public users: User[] = [];
  private isDestroyed = new Subject<boolean>();

  public addButtonText = 'Add Users';
  public removeText = 'Remove User';

  public isIAMv2$: Observable<boolean>;
  public teamId = '';
  public descriptionOrName = '';
  public projects: ProjectCheckedMap = {};
  public unassigned = ProjectConstants.UNASSIGNED_PROJECT_ID;

  constructor(
    private store: Store<NgrxStateAtom>,
    public fb: FormBuilder,
    private router: Router,
    private layoutFacade: LayoutFacadeService
  ) {
    this.updateNameForm = fb.group({
      // Must stay in sync with error checks in team-details.component.html.
      // Also, initialize the form to disabled and enable after team load
      // to prevent people from typing before the team is fetched and have their
      // value overwritten.
      name: new FormControl({value: '', disabled: true},
        [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]),
      projects: [[]]
    });
  }

  ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Settings);

    this.isIAMv2$ = this.store.select(isIAMv2);

    this.isIAMv2$.pipe(takeUntil(this.isDestroyed))
      .subscribe((isV2) => this.descriptionOrName = isV2 ? 'name' : 'description');

    this.store.dispatch(new GetUsers());

    this.store.select(routeState).pipe(
      takeUntil(this.isDestroyed),
      map((state) => [state.params.id as string, state.url]),
      // Only fetch if we are on the team details route, otherwise
      // we'll trigger GetTeam with the wrong input on any route
      // away to a page that also uses the :id param.
      filter(([id, url]) => TEAM_DETAILS_ROUTE.test(url) && id !== undefined),
      // Remove the url because we only need to check if the id has changed
      map(([id, _url]) => id),
      distinctUntilChanged()
    ).subscribe(id => this.store.dispatch(new GetTeam({ id })));

    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
      .subscribe((url: string) => {
        this.url = url;
        const [, fragment] = url.split('#');
        // goes to #users if (1) explicit #users, (2) no fragment, or (3) invalid fragment
        this.tabValue = (fragment === 'details') ? 'details' : 'users';
      });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(updateStatus),
      this.store.select(getTeamUsersStatus)
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([gStatus, uStatus, usersStatus]) => {
      this.isLoadingTeam =
        (gStatus === EntityStatus.loading) ||
        (uStatus === EntityStatus.loading) ||
        (usersStatus === EntityStatus.loading);
      if (this.isLoadingTeam) {
        this.updateNameForm.controls['name'].disable();
      } else {
        this.updateNameForm.controls['name'].enable();
      }
    });

    const team$ = combineLatest([
      this.isIAMv2$,
      this.store.select(v1TeamFromRoute),
      this.store.select(v2TeamFromRoute)
    ]).pipe(
      filter(([isV2, _v1TeamFromRoute, _v2TeamFromRoute]) => isV2 !== null),
      map(([isV2, v1Team, v2Team]) => isV2 ? v2Team : v1Team ));

    combineLatest([this.isIAMv2$, team$]).pipe(
      takeUntil(this.isDestroyed),
      filter(([_isV2, team]) => team !== undefined),
      distinctUntilChanged(([previousIsV2, previousTeam], [currentIsV2, currentTeam]) =>
        previousIsV2 === currentIsV2 && previousTeam === currentTeam)
    ).subscribe(([isV2, team]) => {
      if (isV2) {
        this.teamId = team.id;
      } else {
        this.teamId = team.guid;
      }
      this.team = team;
      this.updateNameForm.controls.name.setValue(this.team.name);
      this.store.dispatch(new GetTeamUsers({ id: this.teamId }));
      if (isV2) {
        this.store.dispatch(new GetProjects());
      }
    });

    combineLatest([
      this.store.select(allProjects),
      this.store.select(getAllProjectStatus),
      team$
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(([_, pStatus, team]) => !pending(pStatus) && !isNil(team))
    ).subscribe(([allowedProjects, _, team]) => {
        this.projects = {};
        allowedProjects
          .forEach(p => {
            this.projects[p.id] = { ...p, checked: team.projects.includes(p.id)
            };
          });
      });

    combineLatest([
      this.store.select(allUsers),
      this.store.select(getAllUsersStatus),
      this.store.select(teamUsers),
      this.store.select(getTeamUsersStatus)]).pipe(
        takeUntil(this.isDestroyed),
        filter(([_allUsers, uStatus, _teamUsers, tuStatus]) =>
            uStatus === EntityStatus.loadingSuccess &&
            tuStatus === EntityStatus.loadingSuccess),
        // Map UUID membership to user records and remove any entries that don't
        // map to user records.
        map(([users, _uStatus, teamUserIds, _tuStatus]) => {
            return at(teamUserIds, keyBy('membership_id', users))
              .filter(userRecord => userRecord !== undefined);
      })).subscribe((users: User[]) => {
        // Sort naturally first by name, then by id
        this.users = ChefSorters.naturalSort(users, ['name', 'id']);
      });

    // handle team update response
    this.store.pipe(
      select(updateStatus),
      takeUntil(this.isDestroyed),
      filter(state => this.saveInProgress && !pending(state)))
      .subscribe((state) => {
        this.saveInProgress = false;
        this.saveSuccessful = (state === EntityStatus.loadingSuccess);
        if (this.saveSuccessful) {
          this.updateNameForm.markAsPristine();
        }
      });
 }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  showUsersTable(): boolean {
    return !this.isLoadingTeam && this.users.length > 0;
  }

  showEmptyStateMessage(): boolean {
    return !this.isLoadingTeam && this.users.length === 0;
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
    this.saveInProgress = true;
    this.updateNameForm.controls['name'].disable();
    const name: string = this.updateNameForm.controls.name.value.trim();
    const projects = Object.keys(this.projects).filter(id => this.projects[id].checked);
    this.store.dispatch(new UpdateTeam({ ...this.team, name, projects }));
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
    return isEmpty(this.projects) || this.saveInProgress;
  }
}
