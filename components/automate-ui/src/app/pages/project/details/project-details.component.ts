import { Component, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Store } from '@ngrx/store';
import { Subject, Observable, combineLatest } from 'rxjs';
import { identity } from 'lodash/fp';
import { find as _find } from 'lodash';
import { filter, map, pluck, takeUntil } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams } from 'app/route.selectors';
import { EntityStatus, loading } from 'app/entities/entities';
import {
  getStatus, updateStatus, projectFromRoute
} from 'app/entities/projects/project.selectors';
import { Project } from 'app/entities/projects/project.model';
import { GetProject, UpdateProject } from 'app/entities/projects/project.actions';
import { GetRules, DeleteRule } from 'app/entities/rules/rule.actions';
import { Rule } from 'app/entities/rules/rule.model';
import {
  allRules
} from 'app/entities/rules/rule.selectors';

@Component({
  selector: 'app-project-details',
  templateUrl: './project-details.component.html',
  styleUrls: ['./project-details.component.scss']
})

export class ProjectDetailsComponent implements OnDestroy {
  public project: Project;
  public projectForm: FormGroup;
  public saveSuccessful = false;
  public isChefManaged = false;
  public rules$: Observable<Rule[]>;
  // public sortedRules$ = [];
  // public sortedRules$ = [
  //   {
  //     name: 'Rule 1',
  //     type: 'Node',
  //     conditions: '3 conditions',
  //     edits: 'Edits Pending'
  //   },
  //   {
  //     name: 'Rule 2',
  //     type: 'Event',
  //     conditions: '1 condition',
  //     edits: 'Applied'
  //   }
  // ];
  public selectedTab: 'rules' | 'details' = 'rules';
  public ruleToDelete: any;
  public deleteModalVisible = false;
  public createModalVisible = false;
  public createProjectForm: FormGroup;
  public creatingProject = false;
  // isLoading represents the initial load as well as subsequent updates in progress.
  public isLoading = true;
  public saving = false;
  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    fb: FormBuilder,
    private store: Store<NgrxStateAtom>
  ) {

    this.projectForm = fb.group({
      name: ['Loading...']
    });

    combineLatest(
      this.store.select(getStatus),
      this.store.select(updateStatus)
    ).pipe(
      takeUntil(this.isDestroyed),
      map(([gStatus, uStatus]) => {
        this.isLoading =
          (gStatus !== EntityStatus.loadingSuccess) ||
          (uStatus === EntityStatus.loading);
      })).subscribe();

    this.store.select(projectFromRoute).pipe(
      filter(identity),
      takeUntil(this.isDestroyed),
      map((state) => {
        this.project = <Project>Object.assign({}, state);
        this.store.dispatch(new GetRules({ project_id: this.project.id }));
        this.rules$ = store.select(allRules);
        this.isChefManaged = this.project.type === 'CHEF_MANAGED';
        this.projectForm = fb.group({
          // Must stay in sync with error checks in project-details.component.html
          name: [this.project.name, Validators.required]
        });
      })).subscribe();

    this.store.select(routeParams).pipe(
      pluck('id'),
      filter(identity),
      takeUntil(this.isDestroyed))
      .subscribe((id: string) => {
        this.store.dispatch(new GetProject({ id }));
      });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  keyPressed() {
    this.saveSuccessful = false;
  }

  onTabChange(event) {
    this.selectedTab = event.target.value;
  }

  closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  startRuleDelete(r: any): void {
    this.deleteModalVisible = true;
    this.ruleToDelete = r;
  }

  deleteRule(): void {
    this.store.dispatch(new DeleteRule({id: this.ruleToDelete.id}));
    this.closeDeleteModal();
  }

  getEditStatus(status: string): string {
    return status === 'staging'
      ? 'Edits pending'
      : 'Applied';
  }

  hasEditsPending(rules: Rule[]): boolean {
    return _find(rules, ['edits', 'staging']) ? true : false;
  }

  // Note: This will be dealt with later, right now, we don't check if it's used
  inUseMessage(): string {
    return '';
  }

  saveProject() {
    this.saveSuccessful = false;
    this.saving = true;
    this.store.dispatch(new UpdateProject({
      id: this.project.id,
      name: this.projectForm.controls['name'].value.trim()
    }));

    const pendingSave = new Subject<boolean>();
    this.store.select(updateStatus).pipe(
      filter(identity),
      takeUntil(pendingSave))
      .subscribe((state) => {
        if (!loading(state)) {
          pendingSave.next(true);
          pendingSave.complete();
          this.saving = false;
          this.saveSuccessful = (state === EntityStatus.loadingSuccess);
        }
      });
  }
}
