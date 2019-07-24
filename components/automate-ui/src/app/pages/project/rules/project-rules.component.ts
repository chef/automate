import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, FormArray, Validators } from '@angular/forms';
import { Router, ActivatedRoute } from '@angular/router';
import { Store } from '@ngrx/store';
import { Subject, combineLatest } from 'rxjs';
import { map, takeUntil, pluck, filter } from 'rxjs/operators';
import { identity } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams } from 'app/route.selectors';
import { EntityStatus, loading } from 'app/entities/entities';
import { Regex } from 'app/helpers/auth/regex';
import { Rule, RuleTypeMappedObject } from 'app/entities/rules/rule.model';
import {
  GetRule,
  GetRulesForProject,
  CreateRule,
  UpdateRule
} from 'app/entities/rules/rule.actions';
import {
  getRuleAttributes,
  getStatus,
  updateStatus,
  ruleFromRoute
} from 'app/entities/rules/rule.selectors';
import { projectFromRoute } from 'app/entities/projects/project.selectors';
import { Project } from 'app/entities/projects/project.model';
import { GetProject } from 'app/entities/projects/project.actions';

@Component({
  selector: 'app-project-rules',
  templateUrl: './project-rules.component.html',
  styleUrls: ['./project-rules.component.scss']
})
export class ProjectRulesComponent implements OnInit, OnDestroy {
  public project: Project = <Project>{};
  public ruleId: string;
  public ruleForm: FormGroup;
  public rule: Rule = <Rule>{};
  public conditions: FormGroup[];
  public isLoading = true;
  public saving = false;
  public attributes: RuleTypeMappedObject;
  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private route: ActivatedRoute,
    private fb: FormBuilder) {

      combineLatest(
        this.store.select(getStatus),
        this.store.select(updateStatus)
      ).pipe(
        takeUntil(this.isDestroyed),
        map(([gStatus, uStatus]) => {
          const routeId = this.route.snapshot.paramMap.get('ruleid');
          this.isLoading =
            routeId
            ? (gStatus !== EntityStatus.loadingSuccess) ||
              (uStatus === EntityStatus.loading)
            : false;
        })).subscribe();

      this.store.select(routeParams).pipe(
        pluck('ruleid'),
        filter(identity),
        takeUntil(this.isDestroyed))
        .subscribe((id: string) => {
          this.store.dispatch(new GetRule({ id }));
        });

      this.store.select(routeParams).pipe(
        pluck('id'),
        filter(identity),
        takeUntil(this.isDestroyed))
        .subscribe((id: string) => {
          this.store.dispatch(new GetProject({ id }));
        });

      this.store.select(ruleFromRoute).pipe(
        filter(identity),
        takeUntil(this.isDestroyed),
        map((state) => {
          this.rule = <Rule>Object.assign({}, state);
        })
        ).subscribe();

      this.store.select(projectFromRoute).pipe(
        filter(identity),
        takeUntil(this.isDestroyed),
        map((state) => {
          this.project = <Project>Object.assign({}, state);
        })
        ).subscribe();

      this.store.select(getRuleAttributes).subscribe((attributes) => {
        this.attributes = attributes;
      });
  }

  ngOnInit(): void {
    this.ruleForm = this.fb.group({
      // Must stay in sync with error checks in project-rules.component.html
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      type: [this.rule.type || '', Validators.required],
      conditions: this.fb.array(this.populateConditions())
    });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  getHeading() {
    return `${this.project.name}: ` + (this.ruleForm.value.name || 'Rule');
  }

  getAttributeLabel() {
    // Important for this to be all lower case for screen readers!
    // (All uppercase is typically read as an acronym.)
    return `${this.ruleForm.get('type').value.toLowerCase()} attribute`;
  }

  backRoute(): string[] {
    return ['/settings', 'projects', this.project.id];
  }

  closePage() {
    this.router.navigate(this.backRoute());
  }

  createCondition(attribute = '', operator = '', values = ''): FormGroup {
    return this.fb.group({
      // Must stay in sync with error checks in project-rules.component.html
      attribute: [attribute, Validators.required],
      operator: [operator, Validators.required],
      values: [values, [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]]
    });
  }

  addCondition(): void {
    const conditions = this.ruleForm.get('conditions') as FormArray;
    conditions.push(this.createCondition());
  }

  deleteCondition(index: number) {
    const conditions = this.ruleForm.get('conditions') as FormArray;
    conditions.removeAt(index);
  }

  showAndLabel(i: number): boolean {
    const conditions = this.ruleForm.get('conditions') as FormArray;
    return (i + 1) < conditions.length;
  }

  showDelete(): boolean {
    const conditions = this.ruleForm.get('conditions') as FormArray;
    return conditions.length > 1;
  }

  populateConditions() {
    this.conditions = [];

    if (this.rule.conditions && this.rule.conditions.length !== 0) {
      this.rule.conditions.forEach(c => {
        this.conditions.push(this.createCondition(c.attribute, c.operator, c.values));
      });
    } else {
      this.conditions.push(this.createCondition());
    }

    return this.conditions;
  }

  updateConditionValues(condition): void {
    if (condition.controls.operator.value === 'MEMBER_OF') {
      condition.controls.values.value =
        (typeof condition.controls.values.value === 'string')
          ? [condition.controls.values.value]
          : condition.controls.values.value.split(',').map((v) => v.trim());
    } else {
      condition.controls.values.value =
        (typeof condition.controls.values.value === 'string')
          ? condition.controls.values.value
          : condition.controls.values.value.join(' ');
    }
  }

  getConditionValue(value): string {
    return (typeof value === 'string') ? value : value.join(', ');
  }

  createRule() {
    this.store.dispatch(new CreateRule({project_id: this.project.id, rule: this.ruleForm.value}));
  }

  updateRule() {
    const updatedRule = this.ruleForm.value;
    updatedRule.id = this.rule.id;
    updatedRule.project_id = this.rule.project_id;
    this.store.dispatch(new UpdateRule({ rule: updatedRule }));
    this.store.dispatch(new GetRulesForProject({ project_id: this.rule.project_id }));
  }

  saveRule() {
    if (this.ruleForm.valid) {
      this.saving = true;
      this.rule.id ? this.updateRule() : this.createRule();
      const pendingSave = new Subject<boolean>();
      this.store.select(updateStatus).pipe(
        filter(identity),
        takeUntil(pendingSave))
        .subscribe((state) => {
          if (!loading(state)) {
            pendingSave.next(true);
            pendingSave.complete();
            this.saving = false;
            this.closePage();
          }
        });
    }
  }
}
