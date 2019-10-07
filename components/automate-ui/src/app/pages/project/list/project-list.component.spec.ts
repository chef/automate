import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { of as observableOf } from 'rxjs';
import { MockComponent } from 'ng2-mock-component';

import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { NgrxStateAtom, runtimeChecks } from 'app/ngrx.reducers';
import { customMatchers } from 'app/testing/custom-matchers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { policyEntityReducer } from 'app/entities/policies/policy.reducer';
import { ProjectService } from 'app/entities/projects/project.service';
import {
  GetProjectsSuccess, GetApplyRulesStatusSuccess, GetApplyRulesStatusSuccessPayload
} from 'app/entities/projects/project.actions';
import { projectEntityReducer, ApplyRulesStatusState } from 'app/entities/projects/project.reducer';
import { Project } from 'app/entities/projects/project.model';
import { ProjectStatus } from 'app/entities/rules/rule.model';
import { ProjectListComponent } from './project-list.component';

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
        MockComponent({ selector: 'app-settings-sidebar' }),
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
          inputs: ['visible', 'applyRulesStatus', 'stopRulesInProgress'],
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
        }, { runtimeChecks })
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

    component.projectsEnabled$ = observableOf(true);
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

    it('does not display project data for less than v2.1', () => {
      component.projectsEnabled$ = observableOf(false);
      store.dispatch(new GetProjectsSuccess({ projects: projectList }));
      expect(element).not.toContainPath('chef-table');
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
      component.openConfirmUpdateStartModal();

      component.cancelApplyStart();

      expect(component.confirmApplyStartModalVisible).toEqual(false);
    });
  });

  describe('when update-start-confirmation modal emits a confirmation', () => {
    beforeEach(() => {
      spyOn(projectService, 'applyRulesStart');
      component.openConfirmUpdateStartModal();
      component.confirmApplyStart();
    });

    it('hides the modal', () => {
      expect(component.confirmApplyStartModalVisible).toEqual(false);
    });

    it('has the projectService start the rule updates', () => {
      expect(projectService.applyRulesStart).toHaveBeenCalled();
    });
  });

  describe('when update-stop-confirmation modal emits a cancellation', () => {
    it('hides the modal', () => {
      component.openConfirmUpdateStopModal();

      component.cancelApplyStop();

      expect(component.confirmApplyStopModalVisible).toEqual(false);
    });
  });

  describe('when update-stop-confirmation modal emits a confirmation', () => {
    beforeEach(() => {
      spyOn(projectService, 'applyRulesStop');
      component.confirmApplyStart(); // start the update
      component.openConfirmUpdateStopModal();
      component.confirmApplyStop(); // emit the confirmation
    });

    it('keeps the modal open', () => {

      expect(component.confirmApplyStopModalVisible).toEqual(true);
    });

    it('has the projectService stop the rule updates', () => {
      expect(projectService.applyRulesStop).toHaveBeenCalled();
    });

    it('waits until stopping the update completes then closes the modal', () => {
      store.dispatch(new GetApplyRulesStatusSuccess(
        genState(ApplyRulesStatusState.NotRunning)));
      expect(component.confirmApplyStopModalVisible).toEqual(false);
    });
  });

  describe('getButtonText', () => {
    it('labels the button "Projects Up-to-Date" when no projects are edited', () => {
      const uneditedProject1 = genProject('uuid-111', 'RULES_APPLIED');
      const uneditedProject2 = genProject('uuid-112', 'RULES_APPLIED');
      store.dispatch(new GetProjectsSuccess({
        projects: [uneditedProject1, uneditedProject2]
      }));

      expect(component.getButtonText()).toEqual('Projects Up-to-Date');
    });

    it('labels the button "Update Projects" when at least one project is edited', () => {
      const editedProject = genProject('uuid-99', 'EDITS_PENDING');
      const uneditedProject1 = genProject('uuid-111', 'RULES_APPLIED');
      const uneditedProject2 = genProject('uuid-112', 'RULES_APPLIED');
      store.dispatch(new GetProjectsSuccess({
        projects: [uneditedProject1, editedProject, uneditedProject2]
      }));

      expect(component.getButtonText()).toEqual('Update Projects');
    });

    it('labels the button with percentage during an update', () => {
      component.confirmApplyStart(); // start the update
      store.dispatch(new GetApplyRulesStatusSuccess( // side effect of the update
        genState(ApplyRulesStatusState.Running)));

      expect(component.getButtonText()).toEqual('Updating Projects 50%...');
    });

    it('labels the button "Update Projects" if update has failed', () => {
      store.dispatch(new GetApplyRulesStatusSuccess(
        genState(ApplyRulesStatusState.NotRunning, true, false)));

      expect(component.getButtonText()).toEqual('Update Projects');
    });

    it('labels the button "Update Projects" if update was cancelled', () => {
      store.dispatch(new GetApplyRulesStatusSuccess(
        genState(ApplyRulesStatusState.NotRunning, false, true)));

      expect(component.getButtonText()).toEqual('Update Projects');
    });
  });

  describe('update projects button', () => {
    it('is disabled if no project has changes', () => {
      const uneditedProject1 = genProject('uuid-111', 'RULES_APPLIED');
      const uneditedProject2 = genProject('uuid-112', 'RULES_APPLIED');
      store.dispatch(new GetProjectsSuccess({ projects: [uneditedProject1, uneditedProject2] }));

      expect(component.isDisabled()).toEqual(true);
    });

    it('is enabled if some project has changes', () => {
      const editedProject = genProject('uuid-99', 'EDITS_PENDING');
      const uneditedProject = genProject('uuid-111', 'RULES_APPLIED');
      store.dispatch(new GetProjectsSuccess({ projects: [uneditedProject, editedProject] }));

      expect(component.isDisabled()).toEqual(false);
    });

    it('is disabled if rules are being applied', () => {
      // isolate rules being applied because button would be enabled with just this
      store.dispatch(
        new GetProjectsSuccess({ projects: [genProject('uuid-99', 'EDITS_PENDING')] }));

      component.confirmApplyStart();

      expect(component.isDisabled()).toEqual(true);
    });

    it('is enabled if rules are not being applied', () => {
      // isolate rules being applied because button would be enabled with just this
      store.dispatch(
        new GetProjectsSuccess({ projects: [genProject('uuid-99', 'EDITS_PENDING')] }));

      component.confirmApplyStart();  // update running
      expect(component.isDisabled()).toEqual(true);
      store.dispatch(new GetApplyRulesStatusSuccess( // update finished
          genState(ApplyRulesStatusState.NotRunning)));

      expect(component.isDisabled()).toEqual(false);
    });

    it('is enabled if update fails', () => {
      store.dispatch(
        new GetProjectsSuccess({ projects: [genProject('uuid-99', 'RULES_APPLIED')] }));
      component.confirmApplyStart();
      store.dispatch(new GetApplyRulesStatusSuccess(
        genState(ApplyRulesStatusState.NotRunning, true, false)));

      expect(component.isDisabled()).toEqual(false);
    });

    it('is enabled if update is cancelled', () => {
      store.dispatch(
        new GetProjectsSuccess({ projects: [genProject('uuid-99', 'RULES_APPLIED')] }));
      component.confirmApplyStart();
      store.dispatch(new GetApplyRulesStatusSuccess(
        genState(ApplyRulesStatusState.NotRunning, false, true)));

      expect(component.isDisabled()).toEqual(false);
    });

    it('is disabled if update is cancelled but update is still running', () => {
      store.dispatch(
        new GetProjectsSuccess({ projects: [genProject('uuid-99', 'RULES_APPLIED')] }));
      component.confirmApplyStart();
      store.dispatch(new GetApplyRulesStatusSuccess(
        genState(ApplyRulesStatusState.Running, false, true)));

      expect(component.isDisabled()).toEqual(true);
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
  state: ApplyRulesStatusState,
  failed = false,
  cancelled = false
): GetApplyRulesStatusSuccessPayload {
  return {
    state,
    failed,
    cancelled,
    estimated_time_complete: '', // unused
    percentage_complete: 0.5, // unused
    failure_message: '' // unused
  };
}
