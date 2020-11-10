import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, FormControl, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Store, select } from '@ngrx/store';
import { keyBy, at, identity, xor } from 'lodash/fp';
import { combineLatest, Subject } from 'rxjs';
import { filter, map, takeUntil, distinctUntilChanged } from 'rxjs/operators';
import { tag } from 'rxjs-spy/operators/tag';

import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { ChefSorters } from 'app/helpers/auth/sorter';
import { Regex } from 'app/helpers/auth/regex';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeURL, routeState } from 'app/route.selectors';
import { EntityStatus, pending } from 'app/entities/entities';
import { User } from 'app/entities/users/user.model';
import { allUsers, getStatus as getAllUsersStatus } from 'app/entities/users/user.selectors';
import { GetUsers } from 'app/entities/users/user.actions';
import { GetProjects } from 'app/entities/projects/project.actions';
import { ProjectConstants } from 'app/entities/projects/project.model';
import {
  teamFromRoute,
  teamUsers,
  getStatus,
  getUsersStatus as getTeamUsersStatus,
  updateStatus
} from 'app/entities/teams/team.selectors';
import { Team } from 'app/entities/teams/team.model';
import {
  GetTeam,
  GetTeamUsers,
  RemoveTeamUsers,
  UpdateTeam
} from 'app/entities/teams/team.actions';

const TEAM_DETAILS_ROUTE = /^\/settings\/teams/;

export type TeamTabName = 'users' | 'details';

@Component({
  selector: 'app-team-details',
  templateUrl: './team-details.component.html',
  styleUrls: ['./team-details.component.scss']
})
export class TeamDetailsComponent implements OnInit, OnDestroy {
  public updateForm: FormGroup;
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

  public teamId = '';
  public unassigned = ProjectConstants.UNASSIGNED_PROJECT_ID;

  constructor(
    private store: Store<NgrxStateAtom>,
    public fb: FormBuilder,
    private router: Router,
    private layoutFacade: LayoutFacadeService
  ) {
    this.updateForm = fb.group({
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
      distinctUntilChanged(),
      tag('team-init-then-dispatch')
    ).subscribe(id => this.store.dispatch(new GetTeam({ id })));

    this.store.select(routeURL).pipe(
      takeUntil(this.isDestroyed),
      tag('team-routeUrl')
    ).subscribe((url: string) => {
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
      takeUntil(this.isDestroyed),
      tag('team-loaded')
    ).subscribe(([gStatus, uStatus, usersStatus]) => {
      this.isLoadingTeam =
        (gStatus === EntityStatus.loading) ||
        (uStatus === EntityStatus.loading) ||
        (usersStatus === EntityStatus.loading);
      if (this.isLoadingTeam) {
        this.updateForm.controls.name.disable();
      } else {
        this.updateForm.controls.name.enable();
      }
    });

    this.store.select(teamFromRoute).pipe(
      takeUntil(this.isDestroyed),
      filter(identity),
      tag('team-fromRoute')
    ).subscribe((team) => {
      this.teamId = team.id;
      this.team = team;
      this.updateForm.controls.name.setValue(this.team.name);
      this.store.dispatch(new GetTeamUsers({ id: this.teamId }));
      this.store.dispatch(new GetProjects());
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
        }),
        tag('team-users')
      ).subscribe((users: User[]) => {
        // Sort naturally first by name, then by id
        this.users = ChefSorters.naturalSort(users, ['name', 'id']);
      });

    // handle team update response
    this.store.pipe(
      select(updateStatus),
      takeUntil(this.isDestroyed),
      filter(state => this.saveInProgress && !pending(state)),
      tag('team-save')
    ).subscribe((state) => {
      this.saveInProgress = false;
      this.saveSuccessful = (state === EntityStatus.loadingSuccess);
      if (this.saveSuccessful) {
        this.updateForm.markAsPristine();
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
    this.store.dispatch(new RemoveTeamUsers({
      id: this.teamId,
      membership_ids: [user.membership_id]
    }));
  }

  saveTeam(): void {
    this.saveSuccessful = false;
    this.saveInProgress = true;
    this.updateForm.controls.name.disable();
    const name: string = this.updateForm.controls.name.value.trim();
    const projects: string[] = this.updateForm.controls.projects.value;
    this.store.dispatch(new UpdateTeam({ ...this.team, name, projects }));
  }

  onSelectedTab(event: { target: { value: TeamTabName } }): void {
    this.tabValue = event.target.value;
    // Drop the previous fragment and add the incoming fragment.
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }

  onProjectDropdownClosing(selectedProjects: string[]): void {

    this.updateForm.controls.projects.setValue(selectedProjects);

    // since the app-projects-dropdown is not a true form input (select)
    // we have to manage the form reactions
    if (xor(this.team.projects, this.updateForm.controls.projects.value).length === 0) {
      this.updateForm.controls.projects.markAsPristine();
    } else {
      this.updateForm.controls.projects.markAsDirty();
    }

  }

}
