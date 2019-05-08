import { Injectable } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { ProjectsFilterOption } from './projects-filter.reducer';
import * as selectors from './projects-filter.selectors';
import { LoadOptions, SaveOptions } from './projects-filter.actions';

const STORE_OPTIONS_KEY = 'projectsFilter.options';

@Injectable()
export class ProjectsFilterService {
  options$ = <Observable<ProjectsFilterOption[]>>this.store.select(selectors.options);

  selectionLabel$ = <Observable<string>>this.store.select(selectors.selectionLabel);

  selectionCount$ = <Observable<number>>this.store.select(selectors.selectionCount);

  selectionCountVisible$ = <Observable<boolean>>this.store.select(selectors.selectionCountVisible);

  selectionCountActive$ = <Observable<boolean>>this.store.select(selectors.selectionCountActive);

  dropdownCaretVisible$ = <Observable<boolean>>this.store.select(selectors.dropdownCaretVisible);

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

}
