import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { Router } from '@angular/router';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';
import { MockChefBreadcrumb, MockChefBreadcrumbs, MockChefButton, MockChefError, MockChefFormField, MockChefHeading, MockChefLoadingSpinner, MockChefOption, MockChefPageHeader, MockChefSubheading, MockChefTabSelector, MockChefTable, MockChefTbody, MockChefTd, MockChefTh, MockChefThead, MockChefToolbar, MockChefTr } from 'app/testing/mock-components';
import { runtimeChecks, ngrxReducers } from 'app/ngrx.reducers';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { GetProjectSuccess } from 'app/entities/projects/project.actions';
import { Project } from 'app/entities/projects/project.model';
import { Rule } from 'app/entities/rules/rule.model';
import { GetRulesSuccess } from 'app/entities/rules/rule.actions';
import { ProjectDetailsComponent } from './project-details.component';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('ProjectDetailsComponent', () => {
  let component: ProjectDetailsComponent;
  let fixture: ComponentFixture<ProjectDetailsComponent>;

  const project: Project = {
    id: 'uuid-1',
    name: 'Default',
    type: 'CHEF_MANAGED',
    status: 'EDITS_PENDING'
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

  beforeEach(waitForAsync(() => {

    TestBed.configureTestingModule({
      declarations: [
        ProjectDetailsComponent
      ],
      imports: [
        ReactiveFormsModule,
        RouterTestingModule,
        ChefPipesModule,
        MockChefToolbar,
        MockComponent({
          selector: 'app-authorized',
          inputs: ['allOf', 'not'],
          template: '<ng-content></ng-content>'
        }),
        MockComponent({
          selector: 'app-delete-object-modal',
          inputs: ['visible', 'objectNoun', 'objectName'],
          outputs: ['close', 'deleteClicked']
        }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),
        MockChefFormField,
        MockChefBreadcrumbs,
        MockChefError,
        MockChefBreadcrumb,
        MockChefTabSelector,
        MockChefButton,
        MockChefHeading,
        MockChefLoadingSpinner,
        MockChefOption,
        MockChefPageHeader,
        MockChefSubheading,
        MockChefTable,
        MockChefThead,
        MockChefTbody,
        MockChefTr,
        MockChefTh,
        MockChefTd,
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
        StoreModule.forRoot({
          ...ngrxReducers,
          router: () => ({
            state: {
              url: '/settings/projects/uuid-1',
              queryParams: {},
              params: {},
              fragment: '',
              path: ['/']
            },
            previousRoute: {},
            navigationId: 0
          })
         }, { runtimeChecks })
      ],
      providers: [
        FeatureFlagsService,
        { provide: TelemetryService, useClass: MockTelemetryService }
      ]
    }).compileComponents();
  }));

  let store: Store<NgrxStateAtom>;
  let router: Router;

  beforeEach(() => {
    store = TestBed.inject(Store);
    router = TestBed.inject(Router);

    store.dispatch(new GetProjectSuccess({
      project: {
        id: 'uuid-1', name: 'Default',
        type: 'CHEF_MANAGED',
        status: 'NO_RULES'
      }
    }));

    store.dispatch(new GetRulesSuccess({
      rules: [],
      status: 'NO_RULES'
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
      spyOn(router, 'navigate');

      component.onSelectedTab({ target: { value: 'details' } });
      expect(component.tabValue).toBe('details');
      expect(router.navigate).toHaveBeenCalled();

      component.onSelectedTab({ target: { value: 'rules' } });
      expect(component.tabValue).toBe('rules');
      expect(router.navigate).toHaveBeenCalled();
    });

    it('does not display rule table', () => {
      expect(component.showRulesTable()).toBeFalsy();
    });

    it('displays create-your-first-rule message', () => {
      expect(component.showNoRulesMessage()).toBeTruthy();
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
      spyOn(router, 'navigate');

      component.onSelectedTab({ target: { value: 'details' } });
      expect(component.tabValue).toBe('details');
      expect(router.navigate).toHaveBeenCalled();

      component.onSelectedTab({ target: { value: 'rules' } });
      expect(component.tabValue).toBe('rules');
      expect(router.navigate).toHaveBeenCalled();
    });

    it('displays rule table', () => {
      expect(component.showRulesTable()).toBeTruthy();
    });

    it('does not display create-your-first-rule message', () => {
      expect(component.showNoRulesMessage()).toBeFalsy();
    });

    it('shows a link back to project when a rule has edits pending', () => {
      expect(component.project.status === 'EDITS_PENDING').toBeTruthy();
    });

    it('enables delete-rule button when a rule has edits pending', () => {
      expect(component.showDeleteRule()).toBeTruthy();
    });
  });
});
