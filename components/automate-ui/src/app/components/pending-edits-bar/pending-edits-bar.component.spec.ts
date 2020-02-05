import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { NgrxStateAtom, runtimeChecks } from 'app/ngrx.reducers';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { customMatchers } from 'app/testing/custom-matchers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { notificationEntityReducer } from 'app/entities/notifications/notification.reducer';
import { clientRunsEntityReducer } from 'app/entities/client-runs/client-runs.reducer';
import { GetIamVersionSuccess } from 'app/entities/policies/policy.actions';
import { policyEntityReducer } from 'app/entities/policies/policy.reducer';
import { Project } from 'app/entities/projects/project.model';
import { ProjectStatus } from 'app/entities/rules/rule.model';
import { ProjectService } from 'app/entities/projects/project.service';
import { projectEntityReducer, ApplyRulesStatusState } from 'app/entities/projects/project.reducer';
import { PendingEditsBarComponent } from './pending-edits-bar.component';
import {
  GetProjectsSuccess,
  GetApplyRulesStatusSuccess,
  GetApplyRulesStatusSuccessPayload
} from 'app/entities/projects/project.actions';

describe('PendingEditsBarComponent', () => {
  let component: PendingEditsBarComponent;
  let fixture: ComponentFixture<PendingEditsBarComponent>;
  let projectService: ProjectService;
  let store: Store<NgrxStateAtom>;

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
          selector: 'app-message-modal',
          inputs: ['visible' ],
          outputs: ['close' ]
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
        MockComponent({ selector: 'mat-progress-bar', inputs: ['mode', 'value', 'bufferValue'] }),
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-table-new' }),
        MockComponent({ selector: 'chef-table-header' }),
        MockComponent({ selector: 'chef-table-body' }),
        MockComponent({ selector: 'chef-table-row' }),
        MockComponent({ selector: 'chef-table-header-cell' }),
        MockComponent({ selector: 'chef-table-cell' }),
        PendingEditsBarComponent
      ],
      imports: [
        ReactiveFormsModule,
        RouterTestingModule,
        ChefPipesModule,
        StoreModule.forRoot(
          {
          policies: policyEntityReducer,
          projects: projectEntityReducer,
          notifications: notificationEntityReducer, // not used here but needed to suppress warnings
          clientRunsEntity: clientRunsEntityReducer // not used here but needed to suppress warnings
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
    fixture = TestBed.createComponent(PendingEditsBarComponent);
    component = fixture.componentInstance;
    store = TestBed.get(Store);

    store.dispatch(new GetIamVersionSuccess({ version: { major: 'v2' } }));
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
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

  describe('pending edits bar', () => {
    it('is hidden if no project has changes', () => {
      const uneditedProject1 = genProject('uuid-111', 'RULES_APPLIED');
      const uneditedProject2 = genProject('uuid-112', 'RULES_APPLIED');
      store.dispatch(new GetProjectsSuccess({ projects: [uneditedProject1, uneditedProject2] }));
      component.updateDisplay();
      expect(component.layoutFacade.layout.userNotifications.pendingEdits).toEqual(false);
    });

    it('is visible if some project has changes', () => {
      const editedProject = genProject('uuid-99', 'EDITS_PENDING');
      const uneditedProject = genProject('uuid-111', 'RULES_APPLIED');
      store.dispatch(new GetProjectsSuccess({ projects: [uneditedProject, editedProject] }));
      component.updateDisplay();
      expect(component.layoutFacade.layout.userNotifications.pendingEdits).toEqual(true);
    });

    it('is visible if rules are not being applied', () => {
      // isolate rules being applied because bar would be visible with just this
      store.dispatch(
        new GetProjectsSuccess({ projects: [genProject('uuid-99', 'EDITS_PENDING')] }));

      component.confirmApplyStart();  // update running
      store.dispatch(new GetApplyRulesStatusSuccess( // update finished
          genState(ApplyRulesStatusState.NotRunning)));
      component.updateDisplay();
      expect(component.layoutFacade.layout.userNotifications.pendingEdits).toEqual(true);
    });

    it('is visible if update fails', () => {
      store.dispatch(
        new GetProjectsSuccess({ projects: [genProject('uuid-99', 'RULES_APPLIED')] }));
      component.confirmApplyStart();
      store.dispatch(new GetApplyRulesStatusSuccess(
        genState(ApplyRulesStatusState.NotRunning, true, false)));
      component.updateDisplay();
      expect(component.layoutFacade.layout.userNotifications.pendingEdits).toEqual(true);
    });

    it('is visible if update is cancelled', () => {
      store.dispatch(
        new GetProjectsSuccess({ projects: [genProject('uuid-99', 'RULES_APPLIED')] }));
      component.confirmApplyStart();
      store.dispatch(new GetApplyRulesStatusSuccess(
        genState(ApplyRulesStatusState.NotRunning, false, true)));
      component.updateDisplay();
      expect(component.layoutFacade.layout.userNotifications.pendingEdits).toEqual(true);
    });

    it('is displayed if update is cancelled but update is still running', () => {
      store.dispatch(
        new GetProjectsSuccess({ projects: [genProject('uuid-99', 'RULES_APPLIED')] }));
      component.confirmApplyStart();
      store.dispatch(new GetApplyRulesStatusSuccess(
        genState(ApplyRulesStatusState.Running, false, true)));
      component.updateDisplay();
      expect(component.layoutFacade.layout.userNotifications.pendingEdits).toEqual(true);
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
