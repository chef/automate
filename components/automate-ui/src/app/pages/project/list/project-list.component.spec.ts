import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { of as observableOf } from 'rxjs';
import { MockComponent } from 'ng2-mock-component';

import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { customMatchers } from 'app/testing/custom-matchers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { IAMMajorVersion, IAMMinorVersion } from 'app/entities/policies/policy.model';
import { policyEntityReducer } from 'app/entities/policies/policy.reducer';
import { ProjectService } from 'app/entities/projects/project.service';
import {
  GetProjectsSuccess, GetApplyRulesStatusSuccess, GetApplyRulesStatusSuccessPayload
} from 'app/entities/projects/project.actions';
import { projectEntityReducer, ApplyRulesStatusState } from 'app/entities/projects/project.reducer';
import { Project } from 'app/entities/projects/project.model';
import { ProjectListComponent } from './project-list.component';
import { ProjectStatus } from 'app/entities/rules/rule.model';

describe('ProjectListComponent', () => {
  let component: ProjectListComponent;
  let fixture: ComponentFixture<ProjectListComponent>;
  let element: HTMLElement;
  let projectService: ProjectService;
  let store: Store<NgrxStateAtom>;

  const projectList: Project[] = [
      {
        id: 'uuid-1', name: 'Default',
        type: 'CHEF_MANAGED',
        status: 'NO_RULES'
      },
      {
        id: 'uuid-2', name: 'another-project',
        type: 'CUSTOM',
        status: 'NO_RULES'
      },
      {
        id: 'uuid-5', name: 'zzz-project',
        type: 'CUSTOM',
        status: 'NO_RULES'
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
          inputs: ['visible', 'objectNoun', 'objectName', 'moreDetails'],
          outputs: ['close', 'deleteClicked']
        }),
        MockComponent({
          selector: 'app-create-object-modal',
          inputs: ['creating', 'createForm', 'visible', 'objectNoun', 'conflictErrorEvent'],
          outputs: ['close', 'deleteClicked']
        }),
        MockComponent({
          selector: 'app-confirm-apply-start-modal',
          inputs: ['visible'],
          outputs: ['confirm', 'cancel']
        }),
        MockComponent({
          selector: 'app-confirm-apply-stop-modal',
          inputs: ['visible', 'applyRulesStatus'],
          outputs: ['confirm', 'cancel']
        }),
        MockComponent({ selector: 'chef-control-menu' }),
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
        ProjectListComponent
      ],
      imports: [
        ReactiveFormsModule,
        RouterTestingModule,
        ChefPipesModule,
        StoreModule.forRoot({
          policies: policyEntityReducer,
          projects: projectEntityReducer
        })
      ],
      providers: [
        FeatureFlagsService,
        ProjectService
      ]
    }).compileComponents();
  }));

  beforeEach(() => {
    jasmine.addMatchers(customMatchers);
    projectService = TestBed.get(ProjectService);
    fixture = TestBed.createComponent(ProjectListComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement.nativeElement;
    store = TestBed.get(Store);

    component.iamMajorVersion$ = observableOf(<IAMMajorVersion>'v2');
    component.iamMinorVersion$ = observableOf(<IAMMinorVersion>'v1');
    fixture.detectChanges();
  });

  describe('when there are no projects', () => {
    it('displays no projects', () => {
      component.sortedProjects$.subscribe(results => {
        expect(results.length).toBe(0);
      });
    });
  });

  describe('when there are projects', () => {

    it('displays project data for v2.1', () => {
      store.dispatch(new GetProjectsSuccess({ projects: projectList }));
      fixture.detectChanges();
      expect(element).toContainPath('chef-table');
      component.sortedProjects$.subscribe(results => {
        expect(results.length).toBe(projectList.length);
        projectList.forEach(p => {
          expect(results.some(result => result.id === p.id)).toBe(true);
        });
      });
    });

    it('does not display project data for v2.0', () => {
      component.iamMajorVersion$ = observableOf(<IAMMajorVersion>'v2');
      component.iamMinorVersion$ = observableOf(<IAMMinorVersion>'v0');
      store.dispatch(new GetProjectsSuccess({ projects: projectList }));
      expect(element).not.toContainPath('chef-table');
    });

    it('does not display project data for v1', () => {
      component.iamMajorVersion$ = observableOf(<IAMMajorVersion>'v1');
      component.iamMinorVersion$ = observableOf(<IAMMinorVersion>'v0');
      store.dispatch(new GetProjectsSuccess({ projects: projectList }));
      fixture.detectChanges();
      expect(element).not.toContainPath('chef-table');
    });

    describe('sortedProject$', () => {
    it('intermixes capitals and lowercase with lowercase first', () => {
      store.dispatch(new GetProjectsSuccess({
        projects: [
          {
            id: 'uuid-1', name: 'Default',
            type: 'CHEF_MANAGED',
            status: 'NO_RULES'
          },
          {
            id: 'uuid-2', name: 'another-project',
            type: 'CUSTOM',
            status: 'NO_RULES'
          },
          {
            id: 'uuid-5', name: 'zzz-project',
            type: 'CUSTOM',
            status: 'NO_RULES'
          }
        ]
      }));
      fixture.detectChanges();
      component.sortedProjects$.subscribe(projects => {
        expect(projects.length).toBe(3);
        expect(projects[0]).toEqual(jasmine.objectContaining({ name: 'another-project' }));
        expect(projects[1]).toEqual(jasmine.objectContaining({ name: 'Default' }));
        expect(projects[2]).toEqual(jasmine.objectContaining({ name: 'zzz-project' }));
      });
    });

    it('sorts by whole string before case', () => {
      store.dispatch(new GetProjectsSuccess({
        projects: [
          {
            id: 'uuid-2', name: 'default',
            type: 'CUSTOM',
            status: 'NO_RULES'
          },
          {
            id: 'uuid-4', name: 'default-resources',
            type: 'CUSTOM',
            status: 'NO_RULES'
          },
          {
            id: 'uuid-5', name: 'Default',
            type: 'CUSTOM',
            status: 'NO_RULES'
          }
        ]
      }));
      fixture.detectChanges();
      component.sortedProjects$.subscribe(projects => {
        expect(projects.length).toBe(3);
        expect(projects[0]).toEqual(jasmine.objectContaining({ name: 'default' }));
        expect(projects[1]).toEqual(jasmine.objectContaining({ name: 'Default' }));
        expect(projects[2]).toEqual(jasmine.objectContaining({ name: 'default-resources' }));
      });
    });

    it('uses natural ordering', () => {
      store.dispatch(new GetProjectsSuccess({
        projects: [
          {
            id: 'uuid-1', name: 'Project01',
            type: 'CHEF_MANAGED',
            status: 'NO_RULES'
          },
          {
            id: 'uuid-2', name: 'Project300',
            type: 'CUSTOM',
            status: 'NO_RULES'
          },
          {
            id: 'uuid-3', name: 'Project3',
            type: 'CUSTOM',
            status: 'NO_RULES'
          },
          {
            id: 'uuid-4', name: 'Project-2',
            type: 'CUSTOM',
            status: 'NO_RULES'
          },
          {
            id: 'uuid-6', name: 'project',
            type: 'CHEF_MANAGED',
            status: 'NO_RULES'
          }
        ]
      }));
      fixture.detectChanges();
      component.sortedProjects$.subscribe(projects => {
        expect(projects.length).toBe(5);
        expect(projects[0]).toEqual(jasmine.objectContaining({ name: 'project' }));
        expect(projects[1]).toEqual(jasmine.objectContaining({ name: 'Project-2' }));
        expect(projects[2]).toEqual(jasmine.objectContaining({ name: 'Project01' }));
        expect(projects[3]).toEqual(jasmine.objectContaining({ name: 'Project3' }));
        expect(projects[4]).toEqual(jasmine.objectContaining({ name: 'Project300' }));
      });
    });
  });

  describe('create modal', () => {
      it('opens upon clicking create button', () => {
        store.dispatch(new GetProjectsSuccess({ projects: projectList }));
        fixture.detectChanges();
        expect(component.createModalVisible).toBe(false);
        (<HTMLButtonElement>(element.querySelector('[data-cy=create-project]'))).click();
        expect(component.createModalVisible).toBe(true);
      });

      it('resets name to empty string', () => {
        store.dispatch(new GetProjectsSuccess({ projects: projectList }));
        fixture.detectChanges();
        component.createProjectForm.controls['name'].setValue('any');
        (<HTMLButtonElement>(element.querySelector('[data-cy=create-project]'))).click();
        expect(component.createProjectForm.controls['name'].value).toBe(null);
      });
    });
  });

  describe('when update-start-confirmation modal emits a cancellation', () => {
    it('hides the modal', () => {
      component.confirmApplyStartModalVisible = true;
      fixture.detectChanges();

      component.cancelApplyStart();
      fixture.detectChanges();

      expect(component.confirmApplyStartModalVisible).toEqual(false);
    });
  });

  describe('when update-start-confirmation modal emits a confirmation', () => {
    beforeEach(() => {
      spyOn(projectService, 'applyRulesStart');
      component.confirmApplyStartModalVisible = true;
      fixture.detectChanges();
    });

    it('hides the modal', () => {
      component.confirmApplyStart();
      fixture.detectChanges();

      expect(component.confirmApplyStartModalVisible).toEqual(false);
    });

    it('has the projectService start the rule updates', () => {
      component.confirmApplyStart();
      fixture.detectChanges();

      expect(projectService.applyRulesStart).toHaveBeenCalled();
    });
  });

  describe('when update-stop-confirmation modal emits a cancellation', () => {
    it('hides the modal', () => {
      component.confirmApplyStopModalVisible = true;
      fixture.detectChanges();

      component.cancelApplyStop();
      fixture.detectChanges();

      expect(component.confirmApplyStopModalVisible).toEqual(false);
    });
  });

  describe('when update-stop-confirmation modal emits a confirmation', () => {
    beforeEach(() => {
      spyOn(projectService, 'applyRulesStop');
      component.confirmApplyStopModalVisible = true;
      fixture.detectChanges();
    });

    it('hides the modal', () => {
      component.confirmApplyStop();
      fixture.detectChanges();

      expect(component.confirmApplyStopModalVisible).toEqual(false);
    });

    it('has the projectService stop the rule updates', () => {
      component.confirmApplyStop();
      fixture.detectChanges();

      expect(projectService.applyRulesStop).toHaveBeenCalled();
    });
  });

  describe('getProjectStatus', () => {
    let editedProject: Project, noRulesProject: Project, uneditedProject: Project;

    beforeEach(() => {
      editedProject = genProject('uuid-99', 'EDITS_PENDING');
      noRulesProject = genProject('uuid-15', 'NO_RULES');
      uneditedProject = genProject('uuid-111', 'RULES_APPLIED');
    });

    it('happy path: needs updating -> updating -> OK', () => {
      store.dispatch(new GetApplyRulesStatusSuccess( // set state
        genState(ApplyRulesStatusState.NotRunning)));
      store.dispatch(new GetProjectsSuccess({ // set cache
        projects: [editedProject]
      }));
      expect(component.getProjectStatus(editedProject)).toBe('Needs updating');

      component.confirmApplyStart(); // now start an update
      store.dispatch(new GetApplyRulesStatusSuccess( // side effect of the update
        genState(ApplyRulesStatusState.Running)));
      expect(component.getProjectStatus(editedProject)).toBe('Updating...');

      // later side effect of the update, but project status NOT affected!
      editedProject.status = 'RULES_APPLIED';
      expect(component.getProjectStatus(editedProject)).toBe('Updating...');

      // update finishes...
      store.dispatch(new GetApplyRulesStatusSuccess(
        genState(ApplyRulesStatusState.NotRunning)));
      // .. and project reflects that all is well!
      expect(component.getProjectStatus(editedProject)).toBe('OK');
    });

    it('continues to report "needs updating" if last update failed', () => {
      store.dispatch(new GetApplyRulesStatusSuccess( // set state
        genState(ApplyRulesStatusState.NotRunning)));
      store.dispatch(new GetProjectsSuccess({ // set cache
        projects: [editedProject]
      }));
      expect(component.getProjectStatus(editedProject)).toBe('Needs updating');

      component.confirmApplyStart(); // now start an update
      store.dispatch(new GetApplyRulesStatusSuccess( // side effect of the update
        genState(ApplyRulesStatusState.Running)));
      expect(component.getProjectStatus(editedProject)).toBe('Updating...');

      // later side effect of the update, but project status NOT affected!
      editedProject.status = 'RULES_APPLIED';
      expect(component.getProjectStatus(editedProject)).toBe('Updating...');

      // update finishes--but this time reporting failure
      store.dispatch(new GetApplyRulesStatusSuccess(
        genState(ApplyRulesStatusState.NotRunning, true)));
      // so we go back to this instead of OK
      expect(component.getProjectStatus(editedProject)).toBe('Needs updating');
    });

    it('happy path for projects with no edits or no rules', () => {
      store.dispatch(new GetApplyRulesStatusSuccess( // set state
        genState(ApplyRulesStatusState.NotRunning)));
      store.dispatch(new GetProjectsSuccess({ // set cache
        projects: [uneditedProject, noRulesProject]
      }));
      // These are unaffected by Running/NotRunning
      expect(component.getProjectStatus(uneditedProject)).toBe('OK');
      expect(component.getProjectStatus(noRulesProject)).toBe('OK');

      component.confirmApplyStart(); // now start an update
      store.dispatch(new GetApplyRulesStatusSuccess( // side effect of the update
        genState(ApplyRulesStatusState.Running)));
      expect(component.getProjectStatus(uneditedProject)).toBe('OK');
      expect(component.getProjectStatus(noRulesProject)).toBe('OK');

      // update finishes...
      store.dispatch(new GetApplyRulesStatusSuccess(
        genState(ApplyRulesStatusState.NotRunning)));
      expect(component.getProjectStatus(uneditedProject)).toBe('OK');
      expect(component.getProjectStatus(noRulesProject)).toBe('OK');
    });

    it('maps current project status while rules are not being applied', () => {
      store.dispatch(new GetApplyRulesStatusSuccess( // set state
        genState(ApplyRulesStatusState.NotRunning)));

      store.dispatch(new GetProjectsSuccess({ // set cache
        projects: [editedProject, uneditedProject, noRulesProject]
      }));

      // Result: this uses current value (EDITS_PENDING)
      expect(component.getProjectStatus(editedProject)).toBe('Needs updating');
      // But the cached value is the same as the current value!
      // So is the above a phantom result?
      // No, because we can affect the answer by changing the current value:
      editedProject.status = 'NO_RULES';
      expect(component.getProjectStatus(editedProject)).toBe('OK');

      // These are unaffected by Running/NotRunning
      expect(component.getProjectStatus(uneditedProject)).toBe('OK');
      expect(component.getProjectStatus(noRulesProject)).toBe('OK');
    });

    it('maps cached status while rules are being applied', () => {
      store.dispatch(new GetApplyRulesStatusSuccess( // set state
        genState(ApplyRulesStatusState.NotRunning)));
      store.dispatch(new GetProjectsSuccess({ // set cache
        projects: [editedProject, uneditedProject, noRulesProject]
      }));
      component.confirmApplyStart(); // now start an update
      store.dispatch(new GetApplyRulesStatusSuccess( // side effect of the update
        genState(ApplyRulesStatusState.Running)));

      // Result: this uses cached value (EDITS_PENDING)
      expect(component.getProjectStatus(editedProject)).toBe('Updating...');
      // But the cached value is the same as the current value!
      // So is the above a phantom result?
      // No, because we still get the same answer even if we change the current value:
      editedProject.status = 'NO_RULES';
      expect(component.getProjectStatus(editedProject)).toBe('Updating...');

      // These are unaffected by Running/NotRunning
      expect(component.getProjectStatus(uneditedProject)).toBe('OK');
      expect(component.getProjectStatus(noRulesProject)).toBe('OK');
    });

    it('does not update cache while rules are being applied', () => {
      store.dispatch(new GetApplyRulesStatusSuccess( // set state
        genState(ApplyRulesStatusState.NotRunning)));
      store.dispatch(new GetProjectsSuccess({ // set cache
        projects: [editedProject, uneditedProject, noRulesProject]
      }));
      component.confirmApplyStart(); // now start an update
      store.dispatch(new GetApplyRulesStatusSuccess( // side effect of the update
        genState(ApplyRulesStatusState.Running)));
      expect(component.getProjectStatus(editedProject)).toBe('Updating...');

      // This would update the cache if we were NotRunning, but does not when Running
      editedProject.status = 'RULES_APPLIED';
      store.dispatch(new GetProjectsSuccess({
        projects: [editedProject, uneditedProject, noRulesProject]
      }));

      // Result: still uses originally cached value, so Updating rather than OK
      expect(component.getProjectStatus(editedProject)).toBe('Updating...');
    });
  });

});

function genProject(id: string, status: ProjectStatus): Project {
  return {
    id,
    status,
    name: id, // unused
    type: 'CUSTOM' // unused
  };
}

function genState(
  state: ApplyRulesStatusState, failed = false): GetApplyRulesStatusSuccessPayload {
  return {
    state,
    failed,
    estimated_time_complete: '', // unused
    percentage_complete: 0.5, // unused
    failure_message: '' // unused
  };
}
