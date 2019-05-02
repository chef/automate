import { Injectable } from '@angular/core';
import { Observable, of as observableOf } from 'rxjs';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { ProjectConstants } from 'app/entities/projects/project.model';
import { ProjectsFilterOption } from './projects-filter.reducer';
import * as selectors from './projects-filter.selectors';
import { LoadOptions, SaveOptions } from './projects-filter.actions';

const STORE_OPTIONS_KEY = 'projectsFilter.options';

@Injectable()
export class ProjectsFilterService {
  options$ = this.store.select(selectors.options);

  selectionLabel$ = this.store.select(selectors.selectionLabel);

  selectionCount$ = this.store.select(selectors.selectionCount);

  selectionCountVisible$ = this.store.select(selectors.selectionCountVisible);

  selectionCountActive$ = this.store.select(selectors.selectionCountActive);

  dropdownCaretVisible$ = this.store.select(selectors.dropdownCaretVisible);

  constructor(private store: Store<NgrxStateAtom>) {}

  loadOptions() {
    this.store.dispatch(new LoadOptions());
  }

  saveOptions(options: ProjectsFilterOption[]) {
    this.store.dispatch(new SaveOptions(options));
  }

  storeOptions(options: ProjectsFilterOption[]) {
    localStorage.setItem(STORE_OPTIONS_KEY, JSON.stringify(options));
  }

  restoreOptions(): ProjectsFilterOption[] {
    return JSON.parse(localStorage.getItem(STORE_OPTIONS_KEY));
  }

  fetchOptions(): Observable<ProjectsFilterOption[]> {
    // Request some options available to the user.
    return observableOf([
      {
        value: 'project-2',
        label: 'Project 2',
        checked: false
      },
      {
        value: 'project-4',
        label: 'Project 4',
        checked: false
      },
      {
        value: ProjectConstants.UNASSIGNED_PROJECT_ID,
        label: 'Unassigned Resources',
        checked: false
      },
      {
        value: 'ze end of the alpha project',
        label: 'ZETA PROJ',
        checked: false
      },
      {
        value: 'project-that-has-a-really-long-name',
        label: 'Project that has a really long name',
        checked: false
      },
      {
        value: 'super-project',
        label: 'Super Duper Project',
        checked: false
      }
    ]);
  }
}
