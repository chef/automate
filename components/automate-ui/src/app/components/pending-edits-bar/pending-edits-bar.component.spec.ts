import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { NgrxStateAtom, runtimeChecks, ngrxReducers } from 'app/ngrx.reducers';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { using } from 'app/testing/spec-helpers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { IndexedEntities } from 'app/entities/entities';
import { UserPermEntity } from 'app/entities/userperms/userperms.entity';
import { PermEntityState, Status } from 'app/entities/userperms/userperms.reducer';
import { ProjectStatus } from 'app/entities/rules/rule.model';
import { Project } from 'app/entities/projects/project.model';
import { ProjectService } from 'app/entities/projects/project.service';
import { ApplyRulesStatusState } from 'app/entities/projects/project.reducer';
import {
  GetProjectsSuccess,
  GetApplyRulesStatusSuccess,
  GetApplyRulesStatusSuccessPayload
} from 'app/entities/projects/project.actions';
import { PendingEditsBarComponent } from './pending-edits-bar.component';

describe('PendingEditsBarComponent', () => {
  let component: PendingEditsBarComponent;
  let fixture: ComponentFixture<PendingEditsBarComponent>;
  let element: HTMLElement;
  let projectService: ProjectService;
  let store: Store<NgrxStateAtom>;

  beforeEach(waitForAsync(() => {
      configureWith({
        '/apis/iam/v2/projects': genPerm('/apis/iam/v2/projects', true)
      });
  }));

  beforeEach(() => {
    projectService = TestBed.inject(ProjectService);
    fixture = TestBed.createComponent(PendingEditsBarComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement.query(By.css('#pending-edits-bar')).nativeElement;
    store = TestBed.inject(Store);
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  describe('update-start-confirmation modal', () => {

    describe('when it emits a cancellation', () => {
      it('it hides the modal', () => {
        component.openConfirmUpdateStartModal();
        component.cancelApplyStart();
        expect(component.confirmApplyStartModalVisible).toEqual(false);
      });
    });

    describe('when it emits a confirmation', () => {
      beforeEach(() => {
        spyOn(projectService, 'applyRulesStart');
        component.openConfirmUpdateStartModal();
        component.confirmApplyStart();
      });

      it('it hides the modal', () => {
        expect(component.confirmApplyStartModalVisible).toEqual(false);
      });

      it('the projectService starts the rule updates', () => {
        expect(projectService.applyRulesStart).toHaveBeenCalled();
      });
    });
  });

  describe('pending edits bar', () => {

    it('is hidden if no project has changes', () => {
      const uneditedProject1 = genProject('uuid-111', 'RULES_APPLIED');
      const uneditedProject2 = genProject('uuid-112', 'RULES_APPLIED');
      store.dispatch(new GetProjectsSuccess({ projects: [uneditedProject1, uneditedProject2] }));
      component.updateDisplay();
      expect(component.isBarHidden).toEqual(true);
    });

    it('is visible if some project has changes', () => {
      const editedProject = genProject('uuid-99', 'EDITS_PENDING');
      const uneditedProject = genProject('uuid-111', 'RULES_APPLIED');
      store.dispatch(new GetProjectsSuccess({ projects: [uneditedProject, editedProject] }));
      component.updateDisplay();
      expect(component.isBarHidden).toEqual(false);
    });

    it('is visible if some project has changes... UNLESS progress bar is active', () => {
      const editedProject = genProject('uuid-99', 'EDITS_PENDING');
      const uneditedProject = genProject('uuid-111', 'RULES_APPLIED');
      store.dispatch(new GetProjectsSuccess({ projects: [uneditedProject, editedProject] }));
      component.updateDisplay();

      component.layoutFacade.layout.userNotifications.updatesProcessing = true;

      expect(component.isBarHidden).toEqual(true);
    });

    it('is visible if rules are not being applied', () => {
      // isolate rules being applied because bar would be visible with just this
      store.dispatch(
        new GetProjectsSuccess({ projects: [genProject('uuid-99', 'EDITS_PENDING')] }));

      component.confirmApplyStart();  // update running
      store.dispatch(new GetApplyRulesStatusSuccess( // update finished
          genState(ApplyRulesStatusState.NotRunning)));
      component.updateDisplay();
      expect(component.isBarHidden).toEqual(false);
    });

    it('is visible if update fails', () => {
      store.dispatch(
        new GetProjectsSuccess({ projects: [genProject('uuid-99', 'RULES_APPLIED')] }));
      component.confirmApplyStart();
      store.dispatch(new GetApplyRulesStatusSuccess(
        genState(ApplyRulesStatusState.NotRunning, true, false)));
      component.updateDisplay();
      expect(component.isBarHidden).toEqual(false);
    });

    it('is visible if update is cancelled', () => {
      store.dispatch(
        new GetProjectsSuccess({ projects: [genProject('uuid-99', 'RULES_APPLIED')] }));
      component.confirmApplyStart();
      store.dispatch(new GetApplyRulesStatusSuccess(
        genState(ApplyRulesStatusState.NotRunning, false, true)));
      component.updateDisplay();
      expect(component.isBarHidden).toEqual(false);
    });

    it('is visible if update is cancelled but update is still running', () => {
      store.dispatch(
        new GetProjectsSuccess({ projects: [genProject('uuid-99', 'RULES_APPLIED')] }));
      component.confirmApplyStart();
      store.dispatch(new GetApplyRulesStatusSuccess(
        genState(ApplyRulesStatusState.Running, false, true)));
      component.updateDisplay();
      expect(component.isBarHidden).toEqual(false);
    });

    it('displays edits pending message before update is applied', () => {
      store.dispatch(
        new GetProjectsSuccess({ projects: [genProject('uuid-99', 'EDITS_PENDING')] }));
      fixture.detectChanges();
      expect(element.textContent).toContain('Project edits pending');
    });

    using([
      ['is cancelled', false, true, 'edits pending'],
      ['fails', true, false, 'update failed'],
      // if it could happen, failure takes precedence:
      ['both fails and is cancelled', true, true, 'update failed']
    ], function (description: string, failed: boolean, cancelled: boolean, message: string) {
      it(`displays "${message}" message when update ${description}`, () => {
        store.dispatch(new GetApplyRulesStatusSuccess(
          genState(ApplyRulesStatusState.NotRunning, failed, cancelled)));
        fixture.detectChanges();
        expect(element.textContent).toContain(`Project ${message}`);
      });
    });

  });
});

describe('PendingEditsBarComponent--unauthorized', () => {
  let component: PendingEditsBarComponent;
  let fixture: ComponentFixture<PendingEditsBarComponent>;
  let store: Store<NgrxStateAtom>;

  beforeEach(waitForAsync(() => {
    configureWith({
      '/apis/iam/v2/projects': genPerm('/apis/iam/v2/projects', false)
    });
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(PendingEditsBarComponent);
    component = fixture.componentInstance;
    store = TestBed.inject(Store);
    fixture.detectChanges();
  });

  // This section contains a copy of the relevant tests from above that are affected
  // when the user is NOT authorized, i.e. all the tests where the bar was previously visible.
  describe('pending edits bar', () => {

    it('is hidden if no project has changes', () => {
      const uneditedProject1 = genProject('uuid-111', 'RULES_APPLIED');
      const uneditedProject2 = genProject('uuid-112', 'RULES_APPLIED');
      store.dispatch(new GetProjectsSuccess({ projects: [uneditedProject1, uneditedProject2] }));
      component.updateDisplay();
      expect(component.isBarHidden).toEqual(true);
    });

    it('is hidden if some project has changes', () => {
      const editedProject = genProject('uuid-99', 'EDITS_PENDING');
      const uneditedProject = genProject('uuid-111', 'RULES_APPLIED');
      store.dispatch(new GetProjectsSuccess({ projects: [uneditedProject, editedProject] }));
      component.updateDisplay();
      expect(component.isBarHidden).toEqual(true);
    });

    it('is hidden if rules are not being applied', () => {
      // isolate rules being applied because bar would be visible with just this
      store.dispatch(
        new GetProjectsSuccess({ projects: [genProject('uuid-99', 'EDITS_PENDING')] }));

      component.confirmApplyStart();  // update running
      store.dispatch(new GetApplyRulesStatusSuccess( // update finished
          genState(ApplyRulesStatusState.NotRunning)));
      component.updateDisplay();
      expect(component.isBarHidden).toEqual(true);
    });

    it('is hidden if update fails', () => {
      store.dispatch(
        new GetProjectsSuccess({ projects: [genProject('uuid-99', 'RULES_APPLIED')] }));
      component.confirmApplyStart();
      store.dispatch(new GetApplyRulesStatusSuccess(
        genState(ApplyRulesStatusState.NotRunning, true, false)));
      component.updateDisplay();
      expect(component.isBarHidden).toEqual(true);
    });

    it('is hidden if update is cancelled', () => {
      store.dispatch(
        new GetProjectsSuccess({ projects: [genProject('uuid-99', 'RULES_APPLIED')] }));
      component.confirmApplyStart();
      store.dispatch(new GetApplyRulesStatusSuccess(
        genState(ApplyRulesStatusState.NotRunning, false, true)));
      component.updateDisplay();
      expect(component.isBarHidden).toEqual(true);
    });

    it('is hidden if update is cancelled but update is still running', () => {
      store.dispatch(
        new GetProjectsSuccess({ projects: [genProject('uuid-99', 'RULES_APPLIED')] }));
      component.confirmApplyStart();
      store.dispatch(new GetApplyRulesStatusSuccess(
        genState(ApplyRulesStatusState.Running, false, true)));
      component.updateDisplay();
      expect(component.isBarHidden).toEqual(true);
    });
  });
});


function configureWith(perms: IndexedEntities<UserPermEntity>): void {
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
        inputs: ['visible'],
        outputs: ['close']
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
      MockComponent({ selector: 'chef-table' }),
      MockComponent({ selector: 'chef-thead' }),
      MockComponent({ selector: 'chef-tbody' }),
      MockComponent({ selector: 'chef-tr' }),
      MockComponent({ selector: 'chef-th' }),
      MockComponent({ selector: 'chef-td' }),
      PendingEditsBarComponent
    ],
    imports: [
      ReactiveFormsModule,
      RouterTestingModule,
      ChefPipesModule,
      StoreModule.forRoot({
        ...ngrxReducers,
          userperms: () => <PermEntityState>{
            status: Status.loadingSuccess,
            byId: perms
          }
      }, { runtimeChecks })
    ],
    providers: [
      FeatureFlagsService,
      ProjectService
    ]
  }).compileComponents();
}

function genPerm(name: string, state: boolean): UserPermEntity {
  return new UserPermEntity(
    { get: state, put: state, post: state, delete: state, patch: state }, name);
}

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
