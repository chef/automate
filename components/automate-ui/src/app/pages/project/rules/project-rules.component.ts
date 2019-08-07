import { Component, OnInit, OnDestroy, EventEmitter } from '@angular/core';
import { FormBuilder, FormGroup, FormArray, Validators } from '@angular/forms';
import { Router, ActivatedRoute } from '@angular/router';
import { Store } from '@ngrx/store';
import { Subject, combineLatest } from 'rxjs';
import { map, takeUntil, pluck, filter } from 'rxjs/operators';
import { identity } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams } from 'app/route.selectors';
import { HttpStatus } from 'app/types/types';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { Regex } from 'app/helpers/auth/regex';
import { EntityStatus, loading } from 'app/entities/entities';
import {
  Rule, RuleTypeMappedObject, Condition, ConditionOperator
} from 'app/entities/rules/rule.model';
import {
  GetRule, GetRulesForProject, CreateRule, UpdateRule
} from 'app/entities/rules/rule.actions';
import {
  getRuleAttributes, getStatus, updateStatus, ruleFromRoute, createError, createStatus
} from 'app/entities/rules/rule.selectors';
import {
  getStatus as getProjectStatus,
  projectFromRoute
} from 'app/entities/projects/project.selectors';
import { Project } from 'app/entities/projects/project.model';
import { GetProject } from 'app/entities/projects/project.actions';

interface KVCondition {
  key: ConditionOperator;
  value: string;
}

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
  public editingRule = false;
  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  // Whether the edit ID form is open or not.
  public modifyID = false;
  public conflictError = false;
  // This element assumes 'id' is the only create field that can conflict.
  private conflictErrorEvent = new EventEmitter<boolean>();

  public operators: KVCondition[] = [
    {
      key: 'EQUALS',
      value: 'equals'
    },
    {
      key: 'MEMBER_OF',
      value: 'member of'
    }
  ];

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private route: ActivatedRoute,
    private fb: FormBuilder) {

      combineLatest(
        this.store.select(getStatus),
        this.store.select(updateStatus),
        this.store.select(getProjectStatus)
      ).pipe(
        takeUntil(this.isDestroyed),
        map(([gStatus, uStatus, gpStatus]: [string, string, string]) => {
          const routeId = this.route.snapshot.paramMap.get('ruleid');
          this.isLoading =
            routeId
            ? (gStatus !== EntityStatus.loadingSuccess) ||
              (uStatus === EntityStatus.loading) ||
              (gpStatus !== EntityStatus.loadingSuccess)
            : false;
        })).subscribe();

      combineLatest(
        this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
        this.store.select(routeParams).pipe(pluck('ruleid'), filter(identity))
      ).pipe(
        takeUntil(this.isDestroyed),
        map(([project_id, rule_id]: [string, string]) => {
          this.store.dispatch(new GetProject({ id: project_id }));
          this.store.dispatch(new GetRule({
            id: rule_id,
            project_id: project_id
          }));
        })).subscribe();

      this.store.select(projectFromRoute).pipe(
        filter(identity),
        takeUntil(this.isDestroyed),
        map((state) => {
          this.project = { ...state };
        })
      ).subscribe();

      this.store.select(ruleFromRoute).pipe(
        filter(identity),
        takeUntil(this.isDestroyed),
        map((state) => {
          this.rule = { ...state };
          this.editingRule = true;
        })
        ).subscribe();

      this.store.select(getRuleAttributes).subscribe((attributes) => {
        this.attributes = attributes;
      });
  }

  ngOnInit(): void {
    this.conflictErrorEvent.subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
      // Open the ID input on conflict so user can resolve it.
      this.modifyID = true;
    });

    if (this.editingRule) {
      this.ruleForm = this.fb.group({
        name: [this.rule.name, [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
        id: [{ value: this.rule.id, disabled: true }], // always disabled, no validation needed
        type: [{ value: this.rule.type, disabled: true }], // always disabled, no validation needed
        conditions: this.fb.array(this.populateConditions())
      });
    } else {
      this.ruleForm = this.fb.group({
        // Must stay in sync with error checks in project-rules.component.html
        name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
        id: ['', [Validators.required, Validators.pattern(Regex.patterns.ID),
        Validators.maxLength(64)]],
        type: ['', Validators.required],
        conditions: this.fb.array(this.populateConditions())
      });
    }
  }

  get getConditions() { return this.ruleForm.get('conditions'); }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  getHeading() {
    return `${this.project.name}: ` + (this.ruleForm.value.name.trim() || 'Rule');
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
        this.conditions.push(
          // Convert values array to display string
          this.createCondition(c.attribute, c.operator, c.values.join(', ')));
      });
    } else {
      this.conditions.push(this.createCondition());
    }

    return this.conditions;
  }

  createRule() {
    this.store.dispatch(
      new CreateRule({
        rule: this.convertToRule()
      }));
  }

  updateRule() {
    const updatedRule = this.convertToRule();
    this.store.dispatch(new UpdateRule({ rule: updatedRule }));
    this.store.dispatch(new GetRulesForProject({ project_id: this.rule.project_id }));
  }

  convertToRule(): Rule {
    const conditions: Condition[] = [];
    // This constant ensures type safety
    const equals_op: ConditionOperator = 'EQUALS';
    this.ruleForm.controls.conditions.value.forEach(c => {
      conditions.push(<Condition>{
        attribute: c.attribute,
        operator: c.operator,
          // Convert values string to storage format
        values: c.operator === equals_op
          ? [c.values.trim()]
          : c.values.split(/,/).map((v: string) => v.trim())
      });
    });
    return {
      project_id: this.project.id,
      id: this.ruleForm.controls.id.value,
      name: this.ruleForm.controls.name.value.trim(),
      type: this.ruleForm.controls.type.value,
      status: 'STAGED',
      conditions: conditions
    };
  }

  // TODO: Leveraged much from ID section of create-object-modal... make a shared component...?
  saveRule() {
    if (this.ruleForm.valid) {
      this.saving = true;
      this.rule.id ? this.updateRule() : this.createRule(); // TODO
      const selector = this.rule.id ? updateStatus : createStatus;
      const pendingSave = new Subject<boolean>();
      this.store.select(selector).pipe(
        filter(identity),
        takeUntil(pendingSave))
        .subscribe((state) => {
          if (!loading(state)) {
            pendingSave.next(true);
            pendingSave.complete();
            this.saving = false;
            if (state === EntityStatus.loadingSuccess) {
              this.closePage();
            } else if (state === EntityStatus.loadingFailure) {
            const pendingCreateError = new Subject<boolean>();
            this.store.select(createError).pipe(
              filter(identity),
              takeUntil(pendingCreateError))
              .subscribe((error) => {
                pendingCreateError.next(true);
                pendingCreateError.complete();
                if (error.status === HttpStatus.CONFLICT) {
                  this.conflictErrorEvent.emit(true);
                } else {
                  // Close on any error other than conflict and display in banner.
                  this.closePage();
                }
            });
            }
          }
        });
    }
  }

  public handleNameInput(event: KeyboardEvent): void {
    if (!this.modifyID && !this.isNavigationKey(event) && !this.editingRule) {
      this.conflictError = false;
      this.ruleForm.controls.id.setValue(
        IdMapper.transform(this.ruleForm.controls.name.value.trim()));
    }

    if (this.editingRule) {
      this.ruleForm.markAsDirty();
    }
  }

  handleIDInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  handleConditionChange(): void {
    if (this.editingRule) {
      // changes on the condition forms do not bubble up to the ruleForm
      // so we explicitly set it to dirty here
      this.ruleForm.markAsDirty();
    }
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }

}
