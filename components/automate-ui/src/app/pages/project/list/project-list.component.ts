import { Component, EventEmitter, OnInit, OnDestroy } from '@angular/core';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { Store, select } from '@ngrx/store';
import { Observable, Subject, combineLatest } from 'rxjs';
import { map, takeUntil, filter } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Regex } from 'app/helpers/auth/regex';
import { ChefSorters } from 'app/helpers/auth/sorter';
import { HttpStatus } from 'app/types/types';
import { loading, EntityStatus } from 'app/entities/entities';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { ProjectStatus } from 'app/entities/rules/rule.model';
import { isIAMv2 } from 'app/entities/policies/policy.selectors';
import { ProjectService } from 'app/entities/projects/project.service';
import {
  allProjects, getAllStatus, createStatus, createError
} from 'app/entities/projects/project.selectors';
import { GetProjects, CreateProject, DeleteProject  } from 'app/entities/projects/project.actions';
import { Project } from 'app/entities/projects/project.model';
import { LoadOptions } from 'app/services/projects-filter/projects-filter.actions';
import { ChefKeyboardEvent } from 'app/types/material-types';

@Component({
  selector: 'app-project-list',
  templateUrl: './project-list.component.html',
  styleUrls: ['./project-list.component.scss']
})
export class ProjectListComponent implements OnInit, OnDestroy {
  public isIAMv2$: Observable<boolean>;
  public sortedProjects$: Observable<Project[]>;
  public projectToDelete: Project;
  public deleteModalVisible = false;
  public messageModalVisible = false;
  public createModalVisible = false;
  public createProjectForm: FormGroup;
  public creatingProject = false;
  public conflictErrorEvent = new EventEmitter<boolean>();
  private isDestroyed = new Subject<boolean>();

  public statusLabel: Record<ProjectStatus, string> = {
      'PROJECT_RULES_STATUS_UNSET': '',
      'NO_RULES': 'No rules',
      'EDITS_PENDING': 'Edits pending',
      'RULES_APPLIED': 'Applied'
  };

  constructor(
    private layoutFacade: LayoutFacadeService,
    private store: Store<NgrxStateAtom>,
    public projects: ProjectService,
    fb: FormBuilder
  ) {
    this.createProjectForm = fb.group({
      // Must stay in sync with error checks in create-object-modal.component.html
      name: ['', Validators.required],
      id: ['',
        [Validators.required, Validators.pattern(Regex.patterns.ID), Validators.maxLength(64)]]
    });
  }

  ngOnInit(): void {
    this.layoutFacade.showSidebar('settings');
    this.projects.getApplyRulesStatus();
    this.store.dispatch(new GetProjects());
    this.store.pipe(
      select(getAllStatus),
      takeUntil(this.isDestroyed),
      map(loading)
    ).subscribe((isLoading) =>
      this.layoutFacade.ShowPageLoading(isLoading)
    );
    this.sortedProjects$ = this.store.select(allProjects).pipe(
      map((unsorted: Project[]) => ChefSorters.naturalSort(unsorted, 'name')));

    this.isIAMv2$ = this.store.select(isIAMv2);

    // handle project creation success response
    this.store.pipe(
      select(createStatus),
      takeUntil(this.isDestroyed),
      filter(state => {
        return this.creatingProject && state === EntityStatus.loadingSuccess;
      }))
      .subscribe(() => {
        this.creatingProject = false;
        this.closeCreateModal();

        // This is issued periodically from projects-filter.effects.ts; we do it now
        // so the user doesn't have to wait.
        this.store.dispatch(new LoadOptions());
      });

    combineLatest([
      this.store.select(createStatus),
      this.store.select(createError)
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(() => this.createModalVisible),
      filter(([state, error]) => state === EntityStatus.loadingFailure && !isNil(error)))
      .subscribe(([_, error]) => {
        if (error.status === HttpStatus.CONFLICT) {
          this.conflictErrorEvent.emit(true);
          this.creatingProject = false;
        } else {
          // close modal on any error other than conflict and display in banner
          this.closeCreateModal();
        }
      });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  public closeMessageModal(): void {
    this.messageModalVisible = false;
  }

  public startProjectDelete($event: ChefKeyboardEvent, p: Project): void {
    if ($event.isUserInput) {
      const deletableStates: ProjectStatus[] = ['EDITS_PENDING', 'RULES_APPLIED'];
      if (deletableStates.includes(p.status)) {
        this.messageModalVisible = true;
      } else {
        this.projectToDelete = p;
        this.deleteModalVisible = true;
      }
    }
  }

  public deleteProject(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteProject({id: this.projectToDelete.id}));
  }

  public createProject(): void {
    this.creatingProject = true;
    const project = {
      id: this.createProjectForm.controls['id'].value,
      name: this.createProjectForm.controls['name'].value.trim()
    };
    this.store.dispatch(new CreateProject(project));
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
    this.conflictErrorEvent.emit(false);
  }

}
