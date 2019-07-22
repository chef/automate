import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { customMatchers } from 'app/testing/custom-matchers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { IAMType } from 'app/entities/policies/policy.model';
import { projectEntityReducer } from 'app/entities/projects/project.reducer';
import { ProjectRulesComponent } from './project-rules.component';
import { Rule } from 'app/entities/rules/rule.model';
import { ruleEntityReducer } from 'app/entities/rules/rule.reducer';

describe('ProjectRulesComponent', () => {
  let component: ProjectRulesComponent;
  let fixture: ComponentFixture<ProjectRulesComponent>;

  const project = {
    id: 'uuid-1', name: 'Default',
    type: <IAMType>'CHEF_MANAGED'
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
    jasmine.addMatchers(customMatchers);
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

    it('the heading should show <Project Name>: Rule', () => {
      const heading = `${component.project.name}: Rule`;
      const compHeading = component.getHeading();
      expect(heading).toBe(compHeading);
    });

    it('the heading should change to <Project Name>: <Rule Name>', () => {
      const ruleName = 'My Rule';
      component.ruleForm.get('name').setValue(ruleName);
      const heading = `${component.project.name}: ${ruleName}`;
      const compHeading = component.getHeading();
      expect(heading).toBe(compHeading);
    });

    it('should have a backroute to project page', () => {
      const backRoute = ['/settings', 'projects', component.project.id];
      const compBackRoute = component.backRoute();
      expect(backRoute).toEqual(compBackRoute);
    });

    it('the formgroup should have one condition', () => {
      const conditions = component.ruleForm.get('conditions').value.length;
      expect(conditions).toBe(1);
    });

    it('should create a condition', () => {
      component.addCondition();
      const conditions = component.ruleForm.get('conditions').value.length;
      expect(conditions).toBe(2);
    });

    it('should delete a condition', () => {
      let conditions = component.ruleForm.get('conditions').value.length;
      expect(conditions).toBe(1);
      component.deleteCondition(0);
      conditions = component.ruleForm.get('conditions').value.length;
      expect(conditions).toBe(0);
    });

    it('should return condition value of string', () => {
      const conditionValue = component.getConditionValue('adasd');
      expect(typeof conditionValue === 'string').toBeTruthy();
    });

    it('should return condition value of array', () => {
      const conditionValue = component.getConditionValue(['adasd']);
      expect(typeof conditionValue === 'string').toBeTruthy();
    });

    it('form should be invalid', () => {
      expect(component.ruleForm.valid).toBeFalsy();
    });

    it('should enable submit when valid', () => {
      component.ruleForm.get('name').setValue('My Rule');
      component.ruleForm.get('type').setValue('NODE');
      component.ruleForm.get('conditions').setValue([
        {
          attribute: 'CHEF_ORGS',
          operator: 'EQUAL',
          values: 'my value'
        }
      ]);
      expect(component.ruleForm.valid).toBeTruthy();
    });

    it('should have attribute label with NODE type', () => {
      component.ruleForm.get('type').setValue('NODE');
      const attributeLabel = component.getAttributeLabel();
      expect(attributeLabel)
        .toBe('node attribute'); // specifically should be lowercase for screen reader
    });

    it('should have attribute label with EVENT type', () => {
      component.ruleForm.get('type').setValue('EVENT');
      const attributeLabel = component.getAttributeLabel();
      expect(attributeLabel)
        .toBe('event attribute'); // specifically should be lowercase for screen reader
    });

    it('should not show add label with one condition', () => {
      const showAndLabel = component.showAndLabel(0);
      expect(showAndLabel).toBeFalsy();
    });

    it('should not show delete button with one condition', () => {
      const showDelete = component.showDelete(0);
      expect(showDelete).toBeFalsy();
    });

    it('should show add label with two conditions', () => {
      component.addCondition();
      const showAndLabel = component.showAndLabel(0);
      expect(showAndLabel).toBeTruthy();
    });

    it('should show delete button with two conditions', () => {
      component.addCondition();
      const showDelete = component.showDelete(0);
      expect(showDelete).toBeTruthy();
    });
  });
});
