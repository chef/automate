import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { IAMType } from 'app/entities/policies/policy.model';
import { GetProjectSuccess } from 'app/entities/projects/project.actions';
import { GetRulesSuccess } from 'app/entities/rules/rule.actions';
import { projectEntityReducer } from 'app/entities/projects/project.reducer';
import { ProjectDetailsComponent, ProjectTabNames } from './project-details.component';

describe('ProjectDetailsComponent', () => {
  let component: ProjectDetailsComponent;
  let fixture: ComponentFixture<ProjectDetailsComponent>;

  const project = {
    id: 'uuid-1', name: 'Default',
    type: <IAMType>'CHEF_MANAGED'
  };
  const rules = [
    {
      id: 'rule-1',
      project_id: 'uuid-1',
      name: 'Rule 1',
      type: 'NODE',
      edits: 'staging',
      conditions: [
        {
          attribute: 'CHEF_ORGS',
          values: 'My value',
          operator: 'EQUALS'
        }
      ]
    },
    {
      id: 'rule-2',
      project_id: 'uuid-1',
      name: 'Rule 2',
      type: 'EVENT',
      edits: 'applied',
      conditions: [
        {
          attribute: 'CHEF_ORGS',
          values: '["My value"]',
          operator: 'MEMBER_OF'
        }
      ]
    }
  ];

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
        MockComponent({ selector: 'chef-error'}),
        MockComponent({ selector: 'chef-breadcrumb', inputs: ['link'] }),
        MockComponent({ selector: 'chef-tab-selector', inputs: ['value'] }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-option' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'ng-container', inputs: ['hidden'] }),
        ProjectDetailsComponent
      ],
      imports: [
        ReactiveFormsModule,
        RouterTestingModule,
        ChefPipesModule,
        StoreModule.forRoot({
          router: () => ({
            state: {
              url: '/projects/uuid-1',
              queryParams: {},
              params: {},
              fragment: '',
              path: ['/']
            },
            previousRoute: {},
            navigationId: 0
          }),
          projects: projectEntityReducer
        })
      ],
      providers: [
        FeatureFlagsService
      ]
    }).compileComponents();
  }));

  let store: Store<NgrxStateAtom>;
  beforeEach(() => {
    store = TestBed.get(Store);

    store.dispatch(new GetProjectSuccess({
      project: {
        id: 'uuid-1', name: 'Default',
        type: <IAMType>'CHEF_MANAGED'
      }
    }));

    fixture = TestBed.createComponent(ProjectDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  describe('when there are no rules', () => {
    beforeEach(() => {
      store.dispatch(new GetRulesSuccess({
          rules: []
      }));
      component.rules = [];
      fixture.detectChanges();
    });

    it('rules array should be empty', () => {
      expect(component.rules.length).toBe(0);
    });

   it('defaults to showing rules section', () => {
      expect(component.showTab(ProjectTabNames.Rules)).toBeTruthy();
      expect(component.showTab(ProjectTabNames.Details)).toBeFalsy();
    });

    it('shows/hides sections when based on selection', () => {
      component.onTabChange({ target: { value: ProjectTabNames.Details } });
      expect(component.showTab(ProjectTabNames.Rules)).toBeFalsy();
      expect(component.showTab(ProjectTabNames.Details)).toBeTruthy();
      component.onTabChange({ target: { value: ProjectTabNames.Rules } });
      expect(component.showTab(ProjectTabNames.Rules)).toBeTruthy();
      expect(component.showTab(ProjectTabNames.Details)).toBeFalsy();
    });

    it('does not display rule table', () => {
      expect(component.showRulesTable()).toBeFalsy();
    });

    it('displays create-your-first-rule message', () => {
      expect(component.showFirstRuleMessage()).toBeTruthy();
    });
  });

  describe('when there are rules', () => {
    beforeEach(() => {
      store.dispatch(new GetRulesSuccess({
          rules: rules
      }));
      component.project = project;
      component.rules = rules;
      fixture.detectChanges();
    });

    it('rules array should have two rules', () => {
      expect(component.rules.length).toBe(2);
    });

    it('defaults to showing rules section', () => {
      expect(component.showTab(ProjectTabNames.Rules)).toBeTruthy();
      expect(component.showTab(ProjectTabNames.Details)).toBeFalsy();
    });

    it('shows/hides sections when based on selection', () => {
      component.onTabChange({ target: { value: ProjectTabNames.Details } });
      expect(component.showTab(ProjectTabNames.Rules)).toBeFalsy();
      expect(component.showTab(ProjectTabNames.Details)).toBeTruthy();
      component.onTabChange({ target: { value: ProjectTabNames.Rules } });
      expect(component.showTab(ProjectTabNames.Rules)).toBeTruthy();
      expect(component.showTab(ProjectTabNames.Details)).toBeFalsy();
    });

    it('displays rule table', () => {
      expect(component.showRulesTable()).toBeTruthy();
    });

    it('does not display create-your-first-rule message', () => {
      expect(component.showFirstRuleMessage()).toBeFalsy();
    });

    it('shows a link back to project when a rule is in "edits pending" state', () => {
      expect(component.showProjectLink()).toBeTruthy();
    });

    it('disable delete rule button when a rule is in "staging" state', () => {
      expect(component.showDeleteRule(component.rules[0])).toBeFalsy();
    });
  });
});
