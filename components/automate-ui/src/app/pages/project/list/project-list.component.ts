import { Component, EventEmitter, OnInit } from '@angular/core';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { Observable, Subject } from 'rxjs';
import { map, takeUntil, filter } from 'rxjs/operators';
import { identity } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { loading, EntityStatus } from 'app/entities/entities';
import { iamMajorVersion, iamMinorVersion } from 'app/entities/policies/policy.selectors';
import {
  allProjects, getAllStatus, createStatus, createError
} from 'app/entities/projects/project.selectors';
import { GetProjects, CreateProject, DeleteProject  } from 'app/entities/projects/project.actions';
import { Project } from 'app/entities/projects/project.model';

const ID_PATTERN = '[0-9a-z-]+';

@Component({
  selector: 'app-project-list',
  templateUrl: './project-list.component.html',
  styleUrls: ['./project-list.component.scss']
})
export class ProjectListComponent implements OnInit {
  public loading$: Observable<boolean>;
  public iamMajorVersion$: Observable<string>;
  public iamMinorVersion$: Observable<string>;
  public sortedProjects$: Observable<Project[]>;
  public projectToDelete: Project;
  public deleteModalVisible = false;
  public createModalVisible = false;
  public createProjectForm: FormGroup;
  public creatingProject = false;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public MAX_PROJECTS = 6;
  public confirmUpdateStartModalVisible = false;
  public projectsUpdating = false;

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    fb: FormBuilder
  ) {
    this.loading$ = store.select(getAllStatus).pipe(map(loading));
    this.sortedProjects$ = store.select(allProjects).pipe(
      map((projects: Project[]) => projects.sort(
        (a, b) => {
          const opts = { numeric: true, sensitivity: 'base' };
          return a.name.localeCompare(b.name, undefined, opts)
            || a.name.localeCompare(b.name, undefined, { numeric: true });
        }
      )));

    this.iamMajorVersion$ = store.select(iamMajorVersion);
    this.iamMinorVersion$ = store.select(iamMinorVersion);

    this.createProjectForm = fb.group({
      name: ['', Validators.required],
      id: ['', [Validators.required, Validators.pattern(ID_PATTERN), Validators.maxLength(64)]]
    });
  }

  ngOnInit(): void {
    this.store.dispatch(new GetProjects());
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
                if (error.status === 409) {
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
    this.confirmUpdateStartModalVisible = true;
  }

  closeConfirmUpdateStartModal() {
    this.confirmUpdateStartModalVisible = false;
  }

  confirmUpdateStart() {
    this.confirmUpdateStartModalVisible = false;
    this.projectsUpdating = true;
  }
}
