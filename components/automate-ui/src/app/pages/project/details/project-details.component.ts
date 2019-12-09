import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { Subject, combineLatest } from 'rxjs';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity, isNil } from 'lodash/fp';

import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams, routeURL } from 'app/route.selectors';
import { Regex } from 'app/helpers/auth/regex';
import { EntityStatus, allLoaded, pending } from 'app/entities/entities';
import {
  getStatus, updateStatus, projectFromRoute
} from 'app/entities/projects/project.selectors';
import { Project } from 'app/entities/projects/project.model';
import { GetProject, UpdateProject } from 'app/entities/projects/project.actions';
import { GetRulesForProject, DeleteRule } from 'app/entities/rules/rule.actions';
import { Rule } from 'app/entities/rules/rule.model';
import {
  allRules,
  getAllStatus as getAllRulesForProjectStatus,
  deleteStatus as deleteRuleStatus
} from 'app/entities/rules/rule.selectors';

export type ProjectTabName = 'rules' | 'details';

@Component({
  selector: 'app-project-details',
  templateUrl: './project-details.component.html',
  styleUrls: ['./project-details.component.scss']
})

export class ProjectDetailsComponent implements OnInit, OnDestroy {
  public project: Project;
  public projectForm: FormGroup;
  public saveSuccessful = false;
  public isChefManaged = false;
  public rules: Rule[] = [];
  public tabValue: ProjectTabName = 'rules';
  public url: string;
  public ruleToDelete: Rule;
  public deleteModalVisible = false;
  public updatingProject = false;
  // isLoading represents the initial load as well as subsequent updates in progress.
  public isLoading = true;
  public saving = false;
  private isDestroyed = new Subject<boolean>();
  private id: string;

  constructor(
    private layoutFacade: LayoutFacadeService,
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private router: Router

  ) { }

  ngOnInit(): void {
    this.layoutFacade.showSidebar('settings');
    // Populate our tabValue from the fragment.
    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
      .subscribe((url: string) => {
        this.url = url;
        const [, fragment] = url.split('#');
        // goes to #rules if (1) explicit #definition, (2) no fragment, or (3) invalid fragment
        this.tabValue = (fragment === 'details') ? 'details' : 'rules';
     });

    this.projectForm = this.fb.group({
      // Must stay in sync with error checks in project-details.component.html
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });

    this.store.select(routeParams).pipe(
      pluck('id'),
      filter(identity),
      takeUntil(this.isDestroyed))
      .subscribe((id: string) => {
        this.id = id;
        this.store.dispatch(new GetProject({ id }));
        this.store.dispatch(new GetRulesForProject({ project_id: id }));
      });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(updateStatus),
      this.store.select(getAllRulesForProjectStatus)
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([getProjectSt, updateSt, getRulesSt]) => {
        this.isLoading =
          !allLoaded([getProjectSt, getRulesSt]) || updateSt === EntityStatus.loading;
      });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(getAllRulesForProjectStatus),
      this.store.select(projectFromRoute),
      this.store.select(allRules)
    ]).pipe(
        filter(([getProjectSt, getRulesSt, _projectState, _allRulesState]) =>
          getProjectSt === EntityStatus.loadingSuccess &&
          getRulesSt === EntityStatus.loadingSuccess),
        filter(([_getProjectSt, _getRulesSt, projectState, allRulesState]) =>
          !isNil(projectState) && !isNil(allRulesState)),
        takeUntil(this.isDestroyed)
      ).subscribe(([_getProjectSt, _getRulesSt, projectState, allRulesState]) => {
        this.project = { ...projectState };
        this.rules = allRulesState;
        this.isChefManaged = this.project.type === 'CHEF_MANAGED';
        this.projectForm.controls['name'].setValue(this.project.name);
      });

    // handle project update response
    this.store.select(updateStatus).pipe(
      takeUntil(this.isDestroyed),
      filter(state => this.saving && !pending(state)))
      .subscribe((state) => {
        this.saving = false;
        this.saveSuccessful = (state === EntityStatus.loadingSuccess);
        if (this.saveSuccessful) {
          this.projectForm.markAsPristine();
        }
      });

    // if a rule gets deleted, we need to refresh the project status
    this.store.select(deleteRuleStatus).pipe(
      filter(status => this.id !== undefined && status === EntityStatus.loadingSuccess),
      takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.store.dispatch(new GetProject({ id: this.id }));
      });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  onSelectedTab(event: { target: { value: ProjectTabName } }) {
    this.tabValue = event.target.value;
    // Drop the previous fragment and add the incoming fragment.
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }

  showNoRulesMessage(): boolean {
    return !this.isLoading && this.rules.length === 0;
  }

  showRulesTable(): boolean {
    return !this.isLoading && this.rules.length > 0;
  }

  closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  startRuleDelete(rule: Rule): void {
    this.deleteModalVisible = true;
    this.ruleToDelete = rule;
  }

  deleteRule(): void {
    this.store.dispatch(new DeleteRule({
      project_id: this.ruleToDelete.project_id,
      id: this.ruleToDelete.id
    }));
    this.closeDeleteModal();
  }

  getEditStatus(rule: Rule): string {
    return rule.status === 'STAGED' ? 'Edits pending' : 'Applied';
  }

  showDeleteRule(): boolean {
    return true; // TODO: return false when *project* status is "updating..."
  }

  saveProject(): void {
    this.saveSuccessful = false;
    this.saving = true;
    this.store.dispatch(new UpdateProject({
      id: this.project.id,
      name: this.projectForm.controls['name'].value.trim()
    }));
  }
}
