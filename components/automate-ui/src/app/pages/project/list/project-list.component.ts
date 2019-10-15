import { Component, EventEmitter, OnInit, OnDestroy } from '@angular/core';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { Store } from '@ngrx/store';
import { interval as observableInterval,  Observable, Subject } from 'rxjs';
import { map, takeUntil, filter, take } from 'rxjs/operators';
import { identity } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import { ChefSorters } from 'app/helpers/auth/sorter';
import { HttpStatus } from 'app/types/types';
import { loading, EntityStatus } from 'app/entities/entities';
import { atLeastV2p1 } from 'app/entities/policies/policy.selectors';
import { iamMajorVersion } from 'app/entities/policies/policy.selectors';
import { IAMMajorVersion } from 'app/entities/policies/policy.model';
import { ProjectService } from 'app/entities/projects/project.service';
import {
  allProjects, getAllStatus, createStatus, createError
} from 'app/entities/projects/project.selectors';
import { GetProjects, CreateProject, DeleteProject  } from 'app/entities/projects/project.actions';
import { Project } from 'app/entities/projects/project.model';
import { ApplyRulesStatus, ApplyRulesStatusState } from 'app/entities/projects/project.reducer';
import { LoadOptions } from 'app/services/projects-filter/projects-filter.actions';

@Component({
  selector: 'app-project-list',
  templateUrl: './project-list.component.html',
  styleUrls: ['./project-list.component.scss']
})
export class ProjectListComponent implements OnInit, OnDestroy {
  public MAX_PROJECTS = 6;
  public loading$: Observable<boolean>;
  public iamMajorVersion$: Observable<IAMMajorVersion>;
  public projectsEnabled$: Observable<boolean>;
  public sortedProjects$: Observable<Project[]>;
  public projectToDelete: Project;
  public deleteModalVisible = false;
  public createModalVisible = false;
  public createProjectForm: FormGroup;
  public creatingProject = false;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public confirmApplyStartModalVisible = false;
  public confirmApplyStopModalVisible = false;

  // This flag governs filling the above cache.
  // The state returned by this.projects.applyRulesStatus$ (Running, NotRunning)
  // is not available soon enough--we need to know the instant the user starts the update.
  private applyRulesInProgress = false;

  // True if there are any rules that have a status of 'EDITS_PENDING'.
  private projectsHaveStagedChanges = false;

  private percentageComplete = 0;

  private updateProjectsFailed = false;
  private updateProjectsCancelled = false;
  public cancelRulesInProgress = false;

  public applyRulesButtonText$: Observable<string>;
  public ApplyRulesStatusState = ApplyRulesStatusState;

  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    public projects: ProjectService,
    fb: FormBuilder
  ) {
    this.loading$ = store.select(getAllStatus).pipe(map(loading));
    this.sortedProjects$ = store.select(allProjects).pipe(
      map((unsorted: Project[]) => ChefSorters.naturalSort(unsorted, 'name')));

    this.iamMajorVersion$ = store.select(iamMajorVersion);
    this.projectsEnabled$ = store.select(atLeastV2p1);

    this.projects.applyRulesStatus$
      .pipe(takeUntil(this.isDestroyed))
      .subscribe(({ state, failed, cancelled, percentageComplete }: ApplyRulesStatus) => {
        if (this.applyRulesInProgress && state === ApplyRulesStatusState.NotRunning) {
          this.cancelRulesInProgress = false;
          this.closeConfirmApplyStopModal();
        }
        this.applyRulesInProgress = state === ApplyRulesStatusState.Running;
        this.updateProjectsFailed = failed;
        this.updateProjectsCancelled = cancelled;
        if (!this.cancelRulesInProgress) {
          this.percentageComplete = percentageComplete;
        }
      });

    store.select(allProjects).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe((projectList: Project[]) => {
      this.projectsHaveStagedChanges = projectList.some(p => p.status === 'EDITS_PENDING');
    });

    this.createProjectForm = fb.group({
      // Must stay in sync with error checks in create-object-modal.component.html
      name: ['', Validators.required],
      id: ['',
        [Validators.required, Validators.pattern(Regex.patterns.ID), Validators.maxLength(64)]]
    });
  }

  ngOnInit(): void {
    this.projects.getApplyRulesStatus();
    this.store.dispatch(new GetProjects());
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  public startProjectDelete(p: Project): void {
    this.deleteModalVisible = true;
    this.projectToDelete = p;
  }

  public deleteProject(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteProject({id: this.projectToDelete.id}));
  }

  // Note: This will be dealt with later, right now, we don't check if it's used
  public inUseMessage(): string {
    return '';
  }

  public createProject(): void {
    this.creatingProject = true;
    const project = {
      id: this.createProjectForm.controls['id'].value,
      name: this.createProjectForm.controls['name'].value.trim()
    };
    this.store.dispatch(new CreateProject(project));

    const pendingCreate = new Subject<boolean>();
    this.store.select(createStatus).pipe(
      filter(identity),
      takeUntil(pendingCreate))
      .subscribe((state) => {
        if (!loading(state)) {
          pendingCreate.next(true);
          pendingCreate.complete();
          this.creatingProject = false;
          if (state === EntityStatus.loadingSuccess) {
            // This is issued periodically from projects-filter.effects.ts; we do it now
            // so the user doesn't have to wait.
            this.store.dispatch(new LoadOptions());
            this.closeCreateModal();
          }
          if (state === EntityStatus.loadingFailure) {
            const pendingCreateError = new Subject<boolean>();
            this.store.select(createError).pipe(
              filter(identity),
              takeUntil(pendingCreateError))
              .subscribe((error) => {
                pendingCreateError.next(true);
                pendingCreateError.complete();
                if (error.status === HttpStatus.CONFLICT) {
                  this.conflictErrorEvent.emit(true);
                } else { // Close the modal on any error other than conflict and display in banner.
                  this.closeCreateModal();
                }
            });
          }
        }
      });
  }

  public openCreateModal(): void {
    this.createModalVisible = true;
    this.resetCreateModal();
  }

  public closeCreateModal(): void {
    this.createModalVisible = false;
    this.resetCreateModal();
  }

  resetCreateModal(): void {
    this.creatingProject = false;
    this.createProjectForm.reset();
  }

  public openConfirmUpdateStartModal(): void {
    this.confirmApplyStartModalVisible = true;
  }

  private closeConfirmApplyStartModal(): void {
    this.confirmApplyStartModalVisible = false;
  }

  public confirmApplyStart(): void {
    this.closeConfirmApplyStartModal();
    this.projects.applyRulesStart();
    this.applyRulesInProgress = true;

    // Rapid sampling for 3 seconds for more responsive UX.
    // If the update is still running, the secondary (active) emitter
    // will check this status at frequent intervals.
    // Once the update completes, the tertiary (dormant) emitter
    // will check this status at INfrequent intervals.
    // (See getActiveApplyRulesStatus$ and getDormantApplyRulesStatus$.)
    observableInterval(250).pipe(take(12)) // 12 x 250ms => 3 seconds
      .subscribe(() => {
        this.projects.getApplyRulesStatus();
      });
  }

  public cancelApplyStart(): void {
    this.closeConfirmApplyStartModal();
  }

  public openConfirmUpdateStopModal(): void {
    this.confirmApplyStopModalVisible = true;
  }

  private closeConfirmApplyStopModal(): void {
    this.confirmApplyStopModalVisible = false;
  }

  public confirmApplyStop(): void {
    this.cancelRulesInProgress = true;
    this.projects.applyRulesStop();
  }

  public cancelApplyStop(): void {
    this.closeConfirmApplyStopModal();
  }

  public getRulesStatus(project: Project): string {
    switch (project.status) {
      case 'NO_RULES': return 'No rules';
      case 'EDITS_PENDING': return 'Edits pending';
      case 'RULES_APPLIED': return 'Applied';
      default: return '';
    }
  }

  public getButtonText(): string {
    if (this.applyRulesInProgress) {
      return `Updating Projects ${Math.round(this.percentageComplete * 100)}%...`;
    }
    if (this.projectsHaveStagedChanges
      || this.updateProjectsCancelled
      || this.updateProjectsFailed) {
      return 'Update Projects';
    }
    return 'Projects Up-to-Date';
  }

  public isDisabled(): boolean {
    return this.applyRulesInProgress ||
      (!this.projectsHaveStagedChanges
        && !this.updateProjectsCancelled
        && !this.updateProjectsFailed);
  }

}
