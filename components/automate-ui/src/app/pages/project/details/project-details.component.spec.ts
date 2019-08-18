import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { GetProjectSuccess } from 'app/entities/projects/project.actions';
import { projectEntityReducer } from 'app/entities/projects/project.reducer';
import { Project } from 'app/entities/projects/project.model';
import { Rule } from 'app/entities/rules/rule.model';
import { ProjectDetailsComponent } from './project-details.component';

describe('ProjectDetailsComponent', () => {
  let component: ProjectDetailsComponent;
  let fixture: ComponentFixture<ProjectDetailsComponent>;

  const project: Project = {
    id: 'uuid-1',
    name: 'Default',
    type: 'CHEF_MANAGED',
    status: 'NO_RULES'
  };
  const rules: Rule[] = [
    {
      id: 'rule-1',
      project_id: 'uuid-1',
      name: 'Rule 1',
      type: 'NODE',
      status: 'STAGED',
      conditions: [
        {
          attribute: 'CHEF_ORGANIZATION',
          values: ['My value'],
          operator: 'EQUALS'
        }
      ]
    },
    {
      id: 'rule-2',
      project_id: 'uuid-1',
      name: 'Rule 2',
      type: 'EVENT',
      status: 'APPLIED',
      conditions: [
        {
          attribute: 'CHEF_ORGANIZATION',
          values: ['Value one', 'Value two'],
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
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-breadcrumbs' }),
        MockComponent({ selector: 'chef-error' }),
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
        type: 'CHEF_MANAGED',
        status: 'NO_RULES'
      }
    }));

    fixture = TestBed.createComponent(ProjectDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  describe('when there are no rules', () => {
    beforeEach(() => {
      component.rules = [];
    });

    it('defaults to showing rules section', () => {
      expect(component.tabValue).toBe('rules');
    });

    it('shows/hides sections when based on selection', () => {
      component.onSelectedTab({ target: { value: 'details' } });
      expect(component.tabValue).toBe('details');

      component.onSelectedTab({ target: { value: 'rules' } });
      expect(component.tabValue).toBe('rules');
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
      component.project = project;
      component.rules = rules;
    });

    it('defaults to showing rules section', () => {
      expect(component.tabValue).toBe('rules');
    });

    it('shows/hides sections when based on selection', () => {
      component.onSelectedTab({ target: { value: 'details' } });
      expect(component.tabValue).toBe('details');
      component.onSelectedTab({ target: { value: 'rules' } });
      expect(component.tabValue).toBe('rules');
    });

    it('displays rule table', () => {
      expect(component.showRulesTable()).toBeTruthy();
    });

    it('does not display create-your-first-rule message', () => {
      expect(component.showFirstRuleMessage()).toBeFalsy();
    });

    it('shows a link back to project when a rule has edits pending', () => {
      expect(component.showProjectLink()).toBeTruthy();
    });

    it('enables delete-rule button when a rule has edits pending', () => {
      expect(component.showDeleteRule()).toBeTruthy();
    });
  });
});
