import { Component, OnInit, OnDestroy, EventEmitter } from '@angular/core';
import { Validators, FormGroup, FormBuilder } from '@angular/forms';
import { Store, select } from '@ngrx/store';
import { Observable, Subject, combineLatest } from 'rxjs';
import { map, filter, takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { ChefSorters } from 'app/helpers/auth/sorter';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { loading, EntityStatus, pending } from 'app/entities/entities';
import { isIAMv2 } from 'app/entities/policies/policy.selectors';
import {
  createError,
  createStatus,
  allTeams,
  getAllStatus } from 'app/entities/teams/team.selectors';
import { Team } from 'app/entities/teams/team.model';
import { CreateTeam, DeleteTeam, GetTeams } from 'app/entities/teams/team.actions';
import { Regex } from 'app/helpers/auth/regex';
import { HttpStatus } from 'app/types/types';
import { assignableProjects } from 'app/services/projects-filter/projects-filter.selectors';
import { ProjectsFilterOption } from 'app/services/projects-filter/projects-filter.reducer';
import { Project, ProjectConstants } from 'app/entities/projects/project.model';
import { ChefKeyboardEvent } from 'app/types/material-types';

@Component({
  selector: 'app-team-management',
  templateUrl: './team-management.component.html',
  styleUrls: ['./team-management.component.scss']
})
export class TeamManagementComponent implements OnInit, OnDestroy {
  public sortedTeams$: Observable<Team[]>;
  public teamToDelete: Team;
  public deleteModalVisible = false;
  public createModalVisible = false;
  public createTeamForm: FormGroup;
  public createV1TeamForm: FormGroup;
  public createV1TeamModalVisible = false;
  public creatingTeam = false;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public dropdownProjects: Project[] = [];
  public unassigned = ProjectConstants.UNASSIGNED_PROJECT_ID;
  public isIAMv2: boolean;

  private isDestroyed = new Subject<boolean>();

 constructor(
    private store: Store<NgrxStateAtom>,
    fb: FormBuilder,
    private layoutFacade: LayoutFacadeService
    ) {
      store.pipe(
        select(getAllStatus),
        takeUntil(this.isDestroyed),
        map(loading)
      ).subscribe((isLoading) =>
        this.layoutFacade.ShowPageLoading(isLoading)
      );

    this.sortedTeams$ = store.select(allTeams).pipe(
      map((teams: Team[]) => ChefSorters.naturalSort(teams, 'id')),
      takeUntil(this.isDestroyed));
    this.createTeamForm = fb.group({
      // Must stay in sync with error checks in create-object-modal.component.html
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      id: ['',
        [Validators.required, Validators.pattern(Regex.patterns.ID), Validators.maxLength(64)]],
      projects: [[]]
    });
    this.createV1TeamForm = fb.group({
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      description: ['',
        [
          Validators.required,
          Validators.pattern(Regex.patterns.NON_BLANK),
          Validators.maxLength(64)
        ]
      ]
    });
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.store.dispatch(new GetTeams());

    this.store.pipe(
      select(isIAMv2),
      takeUntil(this.isDestroyed))
      .subscribe(latest => {
        this.isIAMv2 = latest;
    });

    this.store.select(assignableProjects)
      .subscribe((assignable: ProjectsFilterOption[]) => {
        this.dropdownProjects = assignable.map(p => {
          return <Project>{
            id: p.value,
            name: p.label,
            type: p.type
          };
        });
      });

    // handle team creation success response
    this.store.pipe(
      select(createStatus),
      takeUntil(this.isDestroyed),
      filter(state => {
        return (this.createModalVisible || this.createV1TeamModalVisible) && !pending(state);
      }))
      .subscribe(state => {
        this.creatingTeam = false;
        if (state === EntityStatus.loadingSuccess) {
          this.closeCreateModal();
        }
      });

    // handle team creation failure response
    combineLatest([
      this.store.select(createStatus),
      this.store.select(createError)
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(() => (this.createModalVisible || this.createV1TeamModalVisible)),
      filter (([state, error]) => state === EntityStatus.loadingFailure && !isNil(error)))
      .subscribe(([_, error]) => {
        if (error.status === HttpStatus.CONFLICT) {
          this.conflictErrorEvent.emit(true);
        } else {
          // close modal on any error other than conflict and display in banner
          this.closeCreateModal();
        }
      });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  public startTeamDelete($event: ChefKeyboardEvent, team: Team): void {
    if ($event.isUserInput) {
      this.teamToDelete = team;
      this.deleteModalVisible = true;
    }
  }

  public deleteTeam(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteTeam(this.teamToDelete));
  }

  public createV2Team(): void {
    this.createTeamCommon({
      id: this.createTeamForm.controls.id.value,
      name: this.createTeamForm.controls.name.value.trim(),
      projects: this.createTeamForm.controls.projects.value,
      guid: null
    });
  }

  public createV1Team(): void {
    this.createTeamCommon({
      id: this.createV1TeamForm.controls.name.value,
      name: this.createV1TeamForm.controls.description.value.trim(),
      projects: [],
      guid: null
    });
  }

  public createTeamCommon(team: Team): void {
    this.creatingTeam = true;
    this.store.dispatch(new CreateTeam(team));
  }

  public openCreateModal(): void {
    if (this.isIAMv2) {
      this.createModalVisible = true;
    } else {
      this.createV1TeamModalVisible = true;
    }
    this.resetCreateModal();
  }

  public closeCreateModal(): void {
    this.createV1TeamModalVisible = false;
    this.createModalVisible = false;
    this.resetCreateModal();
  }

  resetCreateModal(): void {
    this.creatingTeam = false;
    this.createTeamForm.reset();
    this.createV1TeamForm.reset();
    this.conflictErrorEvent.emit(false);
  }
}
