import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { ProjectsFilterOption } from './projects-filter.reducer';
import * as selectors from './projects-filter.selectors';
import { InitOptions, LoadOptions, SaveOptions, UpdateSelectionCount } from './projects-filter.actions';

const STORE_OPTIONS_KEY = 'projectsFilter.options';

@Injectable()
export class ProjectsFilterService {
  options$ = <Observable<ProjectsFilterOption[]>>this.store.select(selectors.options);

  selectionLabel$ = <Observable<string>>this.store.select(selectors.selectionLabel);

  selectionCount$ = <Observable<number>>this.store.select(selectors.selectionCount);

  selectionCountVisible$ = <Observable<boolean>>this.store.select(selectors.selectionCountVisible);

  selectionCountActive$ = <Observable<boolean>>this.store.select(selectors.selectionCountActive);

  dropdownCaretVisible$ = <Observable<boolean>>this.store.select(selectors.dropdownCaretVisible);

  filterVisible$ = <Observable<boolean>>this.store.select(selectors.filterVisible);

  constructor(private store: Store<NgrxStateAtom>, private router: Router) { }

  loadOptions() {
    // fast load of project selection just from local storage
    this.store.dispatch(new InitOptions());
    // leisurely load of updated projects from back-end refining above
    this.store.dispatch(new LoadOptions());
  }

  saveOptions(options: ProjectsFilterOption[]) {
    this.store.dispatch(new SaveOptions(options));
  }

  updateSelectionCount(options: ProjectsFilterOption[]) {
    this.store.dispatch(new UpdateSelectionCount(options));
  }

  updateLocalStorage(options: ProjectsFilterOption[]) {
    localStorage.setItem(STORE_OPTIONS_KEY, JSON.stringify(options));
  }

  storeOptions(options: ProjectsFilterOption[]) {
    this.updateLocalStorage(options);

    // To get back to where we are
    const currentUrl = location.pathname + location.search;

    // This is a dummy URL but it must exist in the routing module.
    const reloadUrl = '/reload';

    // The router only routes if the route changes.
    // This first switches to somewhere-that-is-not-here
    // then back to here, so the router will do its thing.
    this.router.navigateByUrl(reloadUrl, { skipLocationChange: true })
      .then(() => this.router.navigateByUrl(currentUrl, { skipLocationChange: true }));
  }

  restoreOptions(): ProjectsFilterOption[] {
    return JSON.parse(localStorage.getItem(STORE_OPTIONS_KEY));
  }

}
