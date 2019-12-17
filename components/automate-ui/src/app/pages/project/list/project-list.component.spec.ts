import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpErrorResponse } from '@angular/common/http';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { using } from 'app/testing/spec-helpers';
import { NgrxStateAtom, runtimeChecks } from 'app/ngrx.reducers';
import { GrpcStatus, HttpStatus } from 'app/types/types';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { customMatchers } from 'app/testing/custom-matchers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { ProjectStatus } from 'app/entities/rules/rule.model';
import { notificationEntityReducer } from 'app/entities/notifications/notification.reducer';
import { clientRunsEntityReducer } from 'app/entities/client-runs/client-runs.reducer';
import { GetIamVersionSuccess } from 'app/entities/policies/policy.actions';
import { policyEntityReducer } from 'app/entities/policies/policy.reducer';
import { ProjectService } from 'app/entities/projects/project.service';
import {
  GetProjectsSuccess,
  GetApplyRulesStatusSuccess,
  GetApplyRulesStatusSuccessPayload,
  DeleteProjectFailure,
  DeleteProjectSuccess
} from 'app/entities/projects/project.actions';
import { projectEntityReducer, ApplyRulesStatusState } from 'app/entities/projects/project.reducer';
import { Project } from 'app/entities/projects/project.model';
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
        MockComponent({
          selector: 'app-delete-object-modal',
          inputs: ['visible', 'objectNoun', 'objectName', 'moreDetails', 'errorText'],
          outputs: ['close', 'deleteClicked']
        }),
        MockComponent({
          selector: 'app-create-object-modal',
          inputs: ['creating', 'createForm', 'visible', 'showProjectsDropdown', 'objectNoun', 'conflictErrorEvent', 'createProjectModal'],
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
        MockComponent({ selector: 'chef-table-new' }),
        MockComponent({ selector: 'chef-table-header' }),
        MockComponent({ selector: 'chef-table-body' }),
        MockComponent({ selector: 'chef-table-row' }),
        MockComponent({ selector: 'chef-table-header-cell' }),
        MockComponent({ selector: 'chef-table-cell' }),
        ProjectListComponent
      ],
      imports: [
        ReactiveFormsModule,
        RouterTestingModule,
        ChefPipesModule,
        StoreModule.forRoot(
          {
          policies: policyEntityReducer,
          projects: projectEntityReducer,
          // not used directly in this component, needed to suppress unit test warnings
          notifications: notificationEntityReducer,
          clientRunsEntity: clientRunsEntityReducer
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

    store.dispatch(new GetIamVersionSuccess({ version: { major: 'v2' } }));
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

    it('displays project data for v2', () => {
      store.dispatch(new GetProjectsSuccess({ projects: projectList }));
      fixture.detectChanges();
      expect(element).toContainPath('chef-table-new');
      component.sortedProjects$.subscribe(results => {
        expect(results.length).toBe(projectList.length);
        projectList.forEach(p => {
          expect(results.some(result => result.id === p.id)).toBe(true);
        });
      });
    });

    it('does not display project data for v1', () => {
      store.dispatch(new GetIamVersionSuccess({ version: { major: 'v1' } }));
      store.dispatch(new GetProjectsSuccess({ projects: projectList }));
      expect(element).not.toContainPath('chef-table-new');
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

    describe('delete modal', () => {

      using([
        ['RULES_APPLIED'],
        ['EDITS_PENDING']
      ], function (status: ProjectStatus) {
        it(`shows error message for ${status}`, () => {
          component.startProjectDelete(genProject('uuid-111', status));
          expect(component.deleteErrorMessage).not.toBe('');
        });
      });

      using([
        ['NO_RULES'],
        ['PROJECT_RULES_STATUS_UNSET']
      ], function (status: ProjectStatus) {
        it(`does not show error message for ${status}`, () => {
          component.startProjectDelete(genProject('uuid-111', status));
          expect(component.deleteErrorMessage).toBe('');
        });
      });

      it('opens upon selecting delete from control menu', () => {
        expect(component.deleteModalVisible).toBe(false);
        component.startProjectDelete(genProject('uuid-111', 'RULES_APPLIED'));
        expect(component.deleteModalVisible).toBe(true);
      });

      it('sets error message upon failed precondition when modal is open', () => {
        const expectedMsg = 'delete failed';
        const httpErrorResponse = new HttpErrorResponse(
          {
            status: HttpStatus.BAD_REQUEST,
            error: { message: expectedMsg, code: GrpcStatus.PRECONDITION_FAILED }
          });
        component.startProjectDelete(genProject('uuid-111', 'RULES_APPLIED'));
        store.dispatch(new DeleteProjectFailure(httpErrorResponse));
        expect(component.deleteErrorMessage).toBe(expectedMsg);
      });

      it('keeps modal open upon failed precondition', () => {
        const expectedMsg = 'delete failed';
        const httpErrorResponse = new HttpErrorResponse(
          {
            status: HttpStatus.BAD_REQUEST,
            error: { message: expectedMsg, code: GrpcStatus.PRECONDITION_FAILED }
          });
        component.startProjectDelete(genProject('uuid-111', 'RULES_APPLIED'));
        expect(component.deleteModalVisible).toBe(true);
        store.dispatch(new DeleteProjectFailure(httpErrorResponse));
        expect(component.deleteModalVisible).toBe(true);
      });

      it('does not set error message upon failed precondition if modal is closed', () => {
        const expectedMsg = 'delete failed';
        const httpErrorResponse = new HttpErrorResponse(
          {
            status: HttpStatus.BAD_REQUEST,
            error: { message: expectedMsg, code: GrpcStatus.PRECONDITION_FAILED }
          });
        store.dispatch(new DeleteProjectFailure(httpErrorResponse));
        expect(component.deleteErrorMessage).toBe('');
      });

      it('resets error message after failure when re-opened later', () => {
        const expectedMsg = 'delete failed';
        const httpErrorResponse = new HttpErrorResponse(
          {
            status: HttpStatus.BAD_REQUEST,
            error: { message: expectedMsg, code: GrpcStatus.PRECONDITION_FAILED }
          });
        component.startProjectDelete(genProject('uuid-111', 'RULES_APPLIED'));
        store.dispatch(new DeleteProjectFailure(httpErrorResponse));
        expect(component.deleteErrorMessage).toBe(expectedMsg);
        component.closeDeleteModal();
        component.startProjectDelete(genProject('uuid-222', 'RULES_APPLIED'));
        expect(component.deleteErrorMessage).toBe('');
      });

      it('does not set error upon failure other than precondition failed', () => {
        const expectedMsg = 'delete failed';
        const httpErrorResponse = new HttpErrorResponse(
          {
            status: HttpStatus.INTERNAL_SERVER_ERROR,
            error: { message: expectedMsg, code: GrpcStatus.INTERNAL }
          });
        component.startProjectDelete(genProject('uuid-111', 'RULES_APPLIED'));
        store.dispatch(new DeleteProjectFailure(httpErrorResponse));
        expect(component.deleteErrorMessage).toBe('');
      });

      it('closes upon failure other than precondition failed', () => {
        const expectedMsg = 'delete failed';
        const httpErrorResponse = new HttpErrorResponse(
          {
            status: HttpStatus.INTERNAL_SERVER_ERROR,
            error: { message: expectedMsg, code: GrpcStatus.INTERNAL }
          });
        component.startProjectDelete(genProject('uuid-111', 'RULES_APPLIED'));
        expect(component.deleteModalVisible).toBe(true);
        store.dispatch(new DeleteProjectFailure(httpErrorResponse));
        expect(component.deleteModalVisible).toBe(false);
      });

      it('does not set error message upon success', () => {
        component.startProjectDelete(genProject('uuid-111', 'RULES_APPLIED'));
        store.dispatch(new DeleteProjectSuccess({id: 'uuid-111'}));
        expect(component.deleteErrorMessage).toBe('');
      });

      it('closes upon success', () => {
        component.startProjectDelete(genProject('uuid-111', 'RULES_APPLIED'));
        expect(component.deleteModalVisible).toBe(true);
        store.dispatch(new DeleteProjectSuccess({id: 'uuid-111'}));
        expect(component.deleteModalVisible).toBe(false);
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
