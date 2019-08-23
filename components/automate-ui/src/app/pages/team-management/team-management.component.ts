import { Component, OnInit, OnDestroy, EventEmitter } from '@angular/core';
import { Validators, FormGroup, FormBuilder } from '@angular/forms';
import { Router } from '@angular/router';
import { Store, select } from '@ngrx/store';
import { Observable, Subject } from 'rxjs';
import { map, filter, takeUntil } from 'rxjs/operators';
import { identity } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { loading, EntityStatus } from 'app/entities/entities';
import { iamMajorVersion, iamMinorVersion } from 'app/entities/policies/policy.selectors';
import { IAMMajorVersion, IAMMinorVersion } from 'app/entities/policies/policy.model';
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

@Component({
  selector: 'app-team-management',
  templateUrl: './team-management.component.html',
  styleUrls: ['./team-management.component.scss']
})
export class TeamManagementComponent implements OnInit, OnDestroy {
  public sortedTeams$: Observable<Team[]>;
  public loading$: Observable<boolean>;
  public teamToDelete: Team;
  public deleteModalVisible = false;
  public createModalVisible = false;
  public createTeamForm: FormGroup;
  public createV1TeamForm: FormGroup;
  public createV1TeamModalVisible = false;
  public creatingTeam = false;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public iamMajorVersion$: Observable<IAMMajorVersion>;
  public iamMinorVersion$: Observable<IAMMinorVersion>;
  public isMajorV1 = true;
  public isMinorV1 = false;
  public dropdownProjects: Project[] = [];
  public unassigned = ProjectConstants.UNASSIGNED_PROJECT_ID;

  private isDestroyed = new Subject<boolean>();

 constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    fb: FormBuilder
    ) {
    this.loading$ = store.select(getAllStatus).pipe(map(loading));
    this.sortedTeams$ = store.select(allTeams).pipe(
      map((teams: Team[]) => teams.sort(
        (a, b) => {
          // See https://stackoverflow.com/a/38641281 for these options
          const opts = { numeric: true, sensitivity: 'base' };
          // TODO: still need to observe case for v1; not needed for v2; simplify when v1 removed.
          // Sort by name then by cased-name, since no other field is useful as a secondary sort;
          // this ensures stable sort with respect to case, so 'a' always comes before 'A'.
          return a.id.localeCompare(b.id, undefined, opts)
          || a.id.localeCompare(b.id, undefined, {numeric: true});
        }
      )),
      takeUntil(this.isDestroyed));
    this.iamMajorVersion$ = store.select(iamMajorVersion);
    this.iamMinorVersion$ = store.select(iamMinorVersion);
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
    this.store.dispatch(new GetTeams());

    this.iamMajorVersion$
      .pipe(takeUntil(this.isDestroyed))
      .subscribe((majorVersion) => {
        if (majorVersion === null) { return; }
          this.isMajorV1 = majorVersion === 'v1';
        });

    this.iamMinorVersion$
      .pipe(takeUntil(this.isDestroyed))
      .subscribe((minorVersion) => {
        if (minorVersion === null) { return; }
          this.isMinorV1 = minorVersion === 'v1';
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
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  public startTeamDelete(team: Team): void {
    this.teamToDelete = team;
    this.deleteModalVisible = true;
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

    const pendingCreate = new Subject<boolean>();
    this.store.pipe(
      select(createStatus),
      filter(identity),
      takeUntil(pendingCreate))
      .subscribe((state) => {
        if (!loading(state)) {
          pendingCreate.next(true);
          pendingCreate.complete();
          this.creatingTeam = false;
          if (state === EntityStatus.loadingSuccess) {
            this.closeCreateModal();
            if (this.isMajorV1) {
              this.router.navigate(['/settings', 'teams', team.name]);
            }
            this.router.navigate(['/settings', 'teams', team.id]);
          }
          if (state === EntityStatus.loadingFailure) {
            const pendingCreateError = new Subject<boolean>();
            this.store.pipe(
              select(createError),
              filter(identity),
              takeUntil(pendingCreateError))
              .subscribe((error) => {
                pendingCreateError.next(true);
                pendingCreateError.complete();
                if (error.status === HttpStatus.CONFLICT) {
                  this.conflictErrorEvent.emit(true);
                // Close the modal on any error other than conflict and display in banner.
                } else {
                  this.closeCreateModal();
                }
            });
          }
        }
      });
  }

  public openCreateModal(): void {
    if (this.isMajorV1) {
      this.createV1TeamModalVisible = true;
    } else {
      this.createModalVisible = true;
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
  }
}
