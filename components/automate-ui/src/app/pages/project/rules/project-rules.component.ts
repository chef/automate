import { Component, OnInit, OnDestroy, EventEmitter } from '@angular/core';
import { FormBuilder, FormGroup, FormArray, Validators } from '@angular/forms';
import { Router, ActivatedRoute } from '@angular/router';
import { Store } from '@ngrx/store';
import { Subject, combineLatest } from 'rxjs';
import { takeUntil, pluck, filter } from 'rxjs/operators';
import { identity, isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { routeParams } from 'app/route.selectors';
import { HttpStatus } from 'app/types/types';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { Regex } from 'app/helpers/auth/regex';
import { EntityStatus, pending } from 'app/entities/entities';
import {
  Rule, RuleTypeMappedObject, Condition, ConditionOperator, isConditionOperator, KVPair
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
  public ruleId: string;
  public ruleForm: FormGroup;

  // FIXME: either make properties optional in interface, or provide them on initialization:
  public project: Project = <Project>{};
  public rule: Rule = <Rule>{};

  public isLoading = true;
  public saving = false;
  public attributeList: KVPair;
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

    combineLatest([
      this.store.select(getStatus),
      this.store.select(updateStatus),
      this.store.select(getProjectStatus)
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([gStatus, uStatus, gpStatus]) => {
      const routeId = this.route.snapshot.paramMap.get('ruleid');
      this.isLoading = routeId &&
        (gStatus !== EntityStatus.loadingSuccess) ||
        (uStatus === EntityStatus.loading) ||
        (gpStatus !== EntityStatus.loadingSuccess);
      });

    combineLatest([
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('ruleid'), filter(identity))
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([project_id, rule_id]: string[]) => {
      this.store.dispatch(new GetProject({ id: project_id }));
      this.store.dispatch(new GetRule({
        id: rule_id,
        project_id
      }));
    });

    this.store.select(projectFromRoute).pipe(
      filter(identity),
      takeUntil(this.isDestroyed)
    ).subscribe(project => {
        this.project = project;
    });

    this.store.select(ruleFromRoute).pipe(
      filter(identity),
      takeUntil(this.isDestroyed)
    ).subscribe(rule => {
        this.rule = rule;
        this.editingRule = true;
    });

    this.store.select(getRuleAttributes).pipe(takeUntil(this.isDestroyed))
    .subscribe(attributes => {
        this.attributes = attributes;
      });

    // handle rule creation/update success
    combineLatest([
      this.store.select(createStatus),
      this.store.select(updateStatus)
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(([cStatus, uStatus]) => this.saving && (!pending(cStatus) || !pending(uStatus))))
      .subscribe(([cStatus, uStatus]) => {
        this.saving = false;
        if (cStatus === EntityStatus.loadingSuccess || uStatus === EntityStatus.loadingSuccess) {
          this.closePage();
        }
      });

    // handle rule creation failure
    combineLatest([
      this.store.select(createStatus),
      this.store.select(createError)
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(([state, error]) => state === EntityStatus.loadingFailure && !isNil(error)))
      .subscribe(([_, error]) => {
        if (error.status === HttpStatus.CONFLICT) {
          this.conflictErrorEvent.emit(true);
        } else {
          // close modal on any error other than conflict and display in banner
          this.closePage();
        }
      });
  }

  ngOnInit(): void {
    this.conflictErrorEvent.pipe(takeUntil(this.isDestroyed))
    .subscribe((isConflict: boolean) => {
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
      this.attributeList = this.attributes[this.rule.type.toLowerCase()];
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

    this.checkTypeChange();
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public checkTypeChange() {
    this.ruleForm.get('type').valueChanges.pipe(takeUntil(this.isDestroyed)).subscribe(
      (type: string) => {
        this.attributeList = this.attributes[type.toLowerCase()];
    });
  }

  getHeading(): string {
    return `${this.project.name}: ` + (this.ruleForm.value.name.trim() || 'Rule');
  }

  getAttributeLabel(): string {
    // Important for this to be all lower case for screen readers!
    // (All uppercase is typically read as an acronym.)
    return `${this.ruleForm.get('type').value.toLowerCase()} attribute`;
  }

  backRoute(): string[] {
    return ['/settings', 'projects', this.project.id];
  }

  closePage(): void {
    this.router.navigate(this.backRoute());
  }

  private createCondition(attribute = '', operator = '', values = ''): FormGroup {
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

  deleteCondition(index: number): void {
    const conditions = this.ruleForm.get('conditions') as FormArray;
    conditions.removeAt(index);
    this.ruleForm.markAsDirty();
  }

  showAndLabel(i: number): boolean {
    const conditions = this.ruleForm.get('conditions') as FormArray;
    return (i + 1) < conditions.length;
  }

  showDelete(): boolean {
    const conditions = this.ruleForm.get('conditions') as FormArray;
    return conditions.length > 1;
  }

  private populateConditions(): FormGroup[] {
    let conditions: FormGroup[];

    if (this.rule.conditions && this.rule.conditions.length !== 0) {
      conditions = this.rule.conditions.map(c =>
        // Convert values array to display string
        this.createCondition(c.attribute, c.operator, c.values.join(', ')));
    } else {
      conditions = [this.createCondition()];
    }

    return conditions;
  }

  private createRule(): void {
    this.store.dispatch(
      new CreateRule({
        rule: this.convertToRule()
      }));
  }

  private updateRule(): void {
    const updatedRule = this.convertToRule();
    this.store.dispatch(new UpdateRule({ rule: updatedRule }));
    this.store.dispatch(new GetRulesForProject({ project_id: this.rule.project_id }));
  }

  convertToRule(): Rule {
    const conditions: Condition[] = this.ruleForm.controls.conditions.value.map(
      (c: {attribute: string, values: string, operator: string}) => {
        // Note(sr): the 'default' here should never happen -- but what should we do? Skip?
        const op = isConditionOperator(c.operator) ? c.operator : 'EQUALS';
        return {
          attribute: c.attribute,
          operator: op,
            // Convert values string to storage format
          values: op === 'EQUALS'
            ? [c.values.trim()]
            : c.values.split(',').map(v => v.trim())
        };
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
  saveRule(): void {
    if (this.ruleForm.valid) {
      this.saving = true;
      this.rule.id ? this.updateRule() : this.createRule();
    }
  }

  public handleNameInput(event: KeyboardEvent): void {
    if (!this.modifyID && !this.isNavigationKey(event) && !this.editingRule) {
      this.conflictError = false;
      this.ruleForm.controls.id.setValue(
        IdMapper.transform(this.ruleForm.controls.name.value.trim()));
    }
  }

  handleIDInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }

}
