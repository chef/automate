import { Component, EventEmitter, OnInit, OnDestroy } from '@angular/core';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { interval as observableInterval,  Observable, Subject } from 'rxjs';
import { map, takeUntil, filter } from 'rxjs/operators';
import { identity } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import { loading, EntityStatus } from 'app/entities/entities';
import { ProjectService } from 'app/entities/projects/project.service';
import { iamMajorVersion, iamMinorVersion } from 'app/entities/policies/policy.selectors';
import { IAMMajorVersion, IAMMinorVersion } from 'app/entities/policies/policy.model';
import {
  allProjects, getAllStatus, createStatus, createError
} from 'app/entities/projects/project.selectors';
import { GetProjects, CreateProject, DeleteProject  } from 'app/entities/projects/project.actions';
import { Project } from 'app/entities/projects/project.model';
import { ApplyRulesStatus, ApplyRulesStatusState } from 'app/entities/projects/project.reducer';
import { HttpStatus } from 'app/types/types';

@Component({
  selector: 'app-project-list',
  templateUrl: './project-list.component.html',
  styleUrls: ['./project-list.component.scss']
})
export class ProjectListComponent implements OnInit, OnDestroy {
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
  public MAX_PROJECTS = 6;
  public confirmApplyStartModalVisible = false;
  public confirmApplyStopModalVisible = false;

  public applyRulesButtonText$: Observable<string>;
  public ApplyRulesStatusState = ApplyRulesStatusState;

  private POLLING_INTERVAL_IN_SECONDS = 10;
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

    this.createProjectForm = fb.group({
      // Must stay in sync with error checks in create-object-modal.component.html
      name: ['', Validators.required],
      id: ['',
        [Validators.required, Validators.pattern(Regex.patterns.ID), Validators.maxLength(64)]]
    });
  }

  ngOnInit(): void {
    // Get status now; periodic checks are already being done globally so no need to poll further
    this.projects.getApplyRulesStatus();

    // Get projects status now and periodically while on this page
    this.store.dispatch(new GetProjects());
    observableInterval(1000 * this.POLLING_INTERVAL_IN_SECONDS).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.store.dispatch(new GetProjects());
      });
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

  openConfirmUpdateStartModal() {
    this.confirmApplyStartModalVisible = true;
  }

  closeConfirmApplyStartModal() {
    this.confirmApplyStartModalVisible = false;
  }

  confirmApplyStart() {
    this.confirmApplyStartModalVisible = false;
    this.projects.applyRulesStart();
  }

  cancelApplyStart() {
    this.closeConfirmApplyStartModal();
  }

  openConfirmUpdateStopModal() {
    this.confirmApplyStopModalVisible = true;
  }

  closeConfirmApplyStopModal() {
    this.confirmApplyStopModalVisible = false;
  }

  confirmApplyStop() {
    this.confirmApplyStopModalVisible = false;
    this.projects.applyRulesStop();
  }

  cancelApplyStop() {
    this.closeConfirmApplyStopModal();
  }

  getRulesStatus(project: Project): string {
    switch (project.status) {
      case 'NO_RULES': return 'No rules';
      case 'EDITS_PENDING': return 'Edits pending';
      case 'RULES_APPLIED': return 'Applied';
      default: return '';
    }
  }
}
