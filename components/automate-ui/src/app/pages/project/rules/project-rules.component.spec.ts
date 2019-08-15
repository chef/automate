import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { using } from 'app/testing/spec-helpers';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { projectEntityReducer } from 'app/entities/projects/project.reducer';
import { Rule, Condition, ConditionOperator, RuleType } from 'app/entities/rules/rule.model';
import { ruleEntityReducer } from 'app/entities/rules/rule.reducer';
import { Project } from 'app/entities/projects/project.model';
import { ProjectRulesComponent } from './project-rules.component';

describe('ProjectRulesComponent', () => {
  let component: ProjectRulesComponent;
  let fixture: ComponentFixture<ProjectRulesComponent>;

  const project: Project = {
    id: 'uuid-1',
    name: 'Default',
    type: 'CHEF_MANAGED',
    status: 'NO_RULES'
  };

  beforeEach(async(() => {

    TestBed.configureTestingModule({
      declarations: [
        MockComponent({
          selector: 'chef-toolbar',
          template: '<ng-content></ng-content>'
        }),
         MockComponent({
          selector: 'app-authorized',
          inputs: ['allOf', 'not'],
          template: '<ng-content></ng-content>'
        }),
        MockComponent({ selector: 'app-admin-sidebar' }),
        MockComponent({
          selector: 'app-delete-object-modal',
          inputs: ['visible', 'objectNoun', 'objectName'],
          outputs: ['close', 'deleteClicked']
        }),
        MockComponent({ selector: 'chef-control-menu' }),
        MockComponent({ selector: 'chef-form-field'}),
        MockComponent({ selector: 'chef-breadcrumbs'}),
        MockComponent({ selector: 'chef-breadcrumb', inputs: ['link'] }),
        MockComponent({ selector: 'chef-tab-selector', inputs: ['value'] }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-input', inputs: ['value'] }),
        MockComponent({ selector: 'chef-error'}),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-option' }),
        MockComponent({ selector: 'chef-page',
          inputs: [
            'subheading',
            'confirm-btn-text',
            'heading',
            'disable-confirm',
            'page-loading',
            'confirm-loading'
          ],
          outputs: ['confirm', 'close']
        }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'ng-container', inputs: ['hidden'] }),
        ProjectRulesComponent
      ],
      imports: [
        ReactiveFormsModule,
        RouterTestingModule,
        ChefPipesModule,
        StoreModule.forRoot({
          router: () => ({
            state: {
              url: '/projects/uuid-1/rules',
              queryParams: {},
              params: {},
              fragment: '',
              path: ['/']
            },
            previousRoute: {},
            navigationId: 0
          }),
          projects: projectEntityReducer,
          rules: ruleEntityReducer
        })
      ],
      providers: [
        FeatureFlagsService
      ]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ProjectRulesComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  describe('when creating a new rule', () => {
    beforeEach(() => {
      component.project = project;
      component.rule = <Rule>{};
      component.isLoading = false;
      fixture.detectChanges();
    });

    it('with no rule name the heading shows "<Project Name>: Rule"', () => {
      const heading = `${component.project.name}: Rule`;
      expect(component.getHeading()).toBe(heading);
    });

    it('with whitespace for rule name the heading shows "<Project Name>: Rule"', () => {
      const ruleName = '   ';
      component.ruleForm.get('name').setValue(ruleName);
      const heading = `${component.project.name}: Rule`;
      expect(component.getHeading()).toBe(heading);
    });

    it('with a rule name the heading shows "<Project Name>: <Rule Name>"', () => {
      const ruleName = 'My Rule';
      component.ruleForm.get('name').setValue(ruleName);
      const heading = `${component.project.name}: ${ruleName}`;
      expect(component.getHeading()).toBe(heading);
    });

    it('should have a backroute to project page', () => {
      const backRoute = ['/settings', 'projects', component.project.id];
      expect(component.backRoute()).toEqual(backRoute);
    });

    it('the formgroup should have one condition', () => {
      const conditions = component.ruleForm.get('conditions').value.length;
      expect(conditions).toBe(1);
    });

    it('upon adding a condition the formgroup includes two conditions', () => {
      component.addCondition();
      const conditions = component.ruleForm.get('conditions').value.length;
      expect(conditions).toBe(2);
    });

    it('upon deleting a condition the condition is removed', () => {
      component.addCondition();
      component.addCondition();
      let conditionCount = component.ruleForm.get('conditions').value.length;
      expect(conditionCount).toBe(3);
      component.deleteCondition(0);
      conditionCount = component.ruleForm.get('conditions').value.length;
      expect(conditionCount).toBe(2);
    });

    it('form should be invalid', () => {
      expect(component.ruleForm.valid).toBeFalsy();
    });

    it('should enable submit when valid', () => {
      component.ruleForm.get('name').setValue('My Rule');
      component.ruleForm.get('id').setValue('my-rule');
      component.ruleForm.get('type').setValue('NODE');
      const operator: ConditionOperator  = 'EQUALS';
      component.ruleForm.get('conditions').setValue([
        {
          attribute: 'CHEF_ORGANIZATION',
          operator: operator,
          values: 'my value'
        }
      ]);
      expect(component.ruleForm.valid).toBeTruthy();
    });

    it('should have attribute label with NODE type', () => {
      const ruleType: RuleType = 'NODE';
      component.ruleForm.get('type').setValue(ruleType);
      const attributeLabel = component.getAttributeLabel();
      expect(attributeLabel)
        .toBe('node attribute'); // specifically should be lowercase for screen reader
    });

    it('should have attribute label with EVENT type', () => {
      const ruleType: RuleType = 'EVENT';
      component.ruleForm.get('type').setValue(ruleType);
      const attributeLabel = component.getAttributeLabel();
      expect(attributeLabel)
        .toBe('event attribute'); // specifically should be lowercase for screen reader
    });

    using([
      ['single word', 'EQUALS', 'word', ['word']],
      ['single word', 'MEMBER_OF', 'word', ['word']],
      ['multiple words', 'EQUALS', 'one two', ['one two']],
      ['multiple words', 'MEMBER_OF', 'one two', ['one two']],
      ['multiple words with commas', 'EQUALS', 'one, two, three', ['one, two, three']],
      ['list with commas', 'MEMBER_OF', 'one, two, three', ['one', 'two', 'three']],
      ['single word with whitespace', 'EQUALS', '    word  ', ['word']],
      ['list with whitespace', 'MEMBER_OF', '   one   ,two,   three   ', ['one', 'two', 'three']]
    ], function (
      description: string, operator: ConditionOperator, values: string, expected: string[]) {
      it('converts condition values with ' + description, () => {
        const uiRule = {
          attribute: 'CHEF_ORGANIZATION',
          operator: operator,
          values: values
        };
        component.ruleForm.get('conditions').setValue([uiRule]);
        const rule = component.convertToRule();
        expect(rule.conditions[0]).toEqual(
          <Condition>{
            attribute: uiRule.attribute,
            operator: uiRule.operator,
            values: expected
          });
      });
    });

    it('should not show "and" label with one condition', () => {
      const showAndLabel = component.showAndLabel(0);
      expect(showAndLabel).toBeFalsy();
    });

    it('should not show delete button with one condition', () => {
      const showDelete = component.showDelete();
      expect(showDelete).toBeFalsy();
    });

    it('should show just one "and" label with two conditions', () => {
      component.addCondition();
      expect(component.showAndLabel(0)).toBeTruthy();
      expect(component.showAndLabel(1)).toBeFalsy();
    });

    it('should show two delete buttons with two conditions', () => {
      component.addCondition();
      expect(component.showDelete()).toBeTruthy();
    });
  });
});
