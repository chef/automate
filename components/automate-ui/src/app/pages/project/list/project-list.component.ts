import { Component, EventEmitter, OnInit, OnDestroy } from '@angular/core';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { interval as observableInterval,  Observable, Subject } from 'rxjs';
import { map, takeUntil, filter, take } from 'rxjs/operators';
import { identity } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import { HttpStatus } from 'app/types/types';
import { loading, EntityStatus } from 'app/entities/entities';
import { iamMajorVersion, iamMinorVersion } from 'app/entities/policies/policy.selectors';
import { IAMMajorVersion, IAMMinorVersion } from 'app/entities/policies/policy.model';
import { ProjectService } from 'app/entities/projects/project.service';
import {
  allProjects, getAllStatus, createStatus, createError
} from 'app/entities/projects/project.selectors';
import { GetProjects, CreateProject, DeleteProject  } from 'app/entities/projects/project.actions';
import { Project } from 'app/entities/projects/project.model';
import { ApplyRulesStatus, ApplyRulesStatusState } from 'app/entities/projects/project.reducer';
import { ProjectStatus } from 'app/entities/rules/rule.model';
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
  public iamMinorVersion$: Observable<IAMMinorVersion>;
  public sortedProjects$: Observable<Project[]>;
  public projectToDelete: Project;
  public deleteModalVisible = false;
  public createModalVisible = false;
  public createProjectForm: FormGroup;
  public creatingProject = false;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public confirmApplyStartModalVisible = false;
  public confirmApplyStopModalVisible = false;

  // During an update the Project Update Status needs to show "updating..."
  // for any project whose status is "edits pending" at the moment the update starts.
  // Very shortly after the update starts, though, the project's live status changes
  // from "edits pending" to "applied" so we have lost that initial status.
  // This statusCache, then, remembers those initial status values during the update.
  private statusCache: {  [id: string]: ProjectStatus; } = {};

  // This flag governs filling the above cache.
  // The state returned by this.projects.applyRulesStatus$ (Running, NotRunning)
  // is not available soon enough--we need to know the instant the user starts the update.
  private applyRulesInProgress = false;

  private updateProjectsFailed = false;
  private updateProjectsCancelled = false;

  public applyRulesButtonText$: Observable<string>;
  public ApplyRulesStatusState = ApplyRulesStatusState;

  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    public projects: ProjectService,
    fb: FormBuilder
  ) {
    this.loading$ = store.select(getAllStatus).pipe(map(loading));
    this.sortedProjects$ = store.select(allProjects).pipe(
      map((unsorted: Project[]) => unsorted.sort(
        (a, b) => {
          const opts = { numeric: true, sensitivity: 'base' };
          return a.name.localeCompare(b.name, undefined, opts)
            || a.name.localeCompare(b.name, undefined, { numeric: true });
        }
      )));

    this.iamMajorVersion$ = store.select(iamMajorVersion);
    this.iamMinorVersion$ = store.select(iamMinorVersion);

    this.applyRulesButtonText$ = this.projects.applyRulesStatus$.pipe(
      map(({ state, percentageComplete }: ApplyRulesStatus) => {
        switch (state) {
          case ApplyRulesStatusState.NotRunning:
            return 'Update Projects';
          case ApplyRulesStatusState.Running:
            return `Updating Projects ${Math.round(percentageComplete * 100)}%...`;
        }
      })
    );

    this.projects.applyRulesStatus$
      .subscribe(({ state, failed, cancelled }: ApplyRulesStatus) => {
        if (state === ApplyRulesStatusState.NotRunning) {
          this.applyRulesInProgress = false;
          this.updateProjectsFailed = failed;
          this.updateProjectsCancelled = cancelled;
        }
      });

    store.select(allProjects).pipe(
      takeUntil(this.isDestroyed),
      // do not update this cache while an update is in progress
      filter(() => !this.applyRulesInProgress)
    ).subscribe((projectList: Project[]) => {
      this.statusCache = projectList.reduce((m, p) => ({ ...m, [p.id]: p.status }), {});
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
            this.router.navigate(['/settings', 'projects', project.id]);
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
    this.closeConfirmApplyStopModal();
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

  public getProjectStatus(project: Project): string {
    const cachedStatus = this.statusCache[project.id];
    let result: string;
    if (this.applyRulesInProgress) {
      result = cachedStatus === 'EDITS_PENDING' ? 'Updating...' : 'OK';
    } else {
      result = project.status === 'EDITS_PENDING'
        || this.updateProjectsFailed
        || this.updateProjectsCancelled
        ? 'Needs updating' : 'OK';
    }
    // TODO: check how often this is hit
    return result;
  }
}
