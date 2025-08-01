import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';
import { MockChefButton, MockChefHeading, MockChefLoadingSpinner, MockChefPageHeader, MockChefSubheading, MockChefTable, MockChefTbody, MockChefTd, MockChefTh, MockChefThead, MockChefToolbar, MockChefTr } from 'app/testing/mock-components';
import { NgrxStateAtom, runtimeChecks, ngrxReducers } from 'app/ngrx.reducers';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { customMatchers } from 'app/testing/custom-matchers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { ProjectService } from 'app/entities/projects/project.service';
import { ProcessProgressBarComponent } from './process-progress-bar.component';
import { ApplyRulesStatusState } from 'app/entities/projects/project.reducer';
import {
  GetApplyRulesStatusSuccessPayload,
  GetApplyRulesStatusSuccess
} from 'app/entities/projects/project.actions';

describe('ProcessProgressBarComponent', () => {
  let component: ProcessProgressBarComponent;
  let fixture: ComponentFixture<ProcessProgressBarComponent>;
  let projectService: ProjectService;
  let store: Store<NgrxStateAtom>;

  beforeEach(waitForAsync(() => {

    TestBed.configureTestingModule({
      declarations: [
        ProcessProgressBarComponent
      ],
      imports: [
        ReactiveFormsModule,
        MockChefToolbar,
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
        MockChefButton,
        MockChefHeading,
        MockChefLoadingSpinner,
        MockChefPageHeader,
        MockChefSubheading,
        MockChefTable,
        MockChefThead,
        MockChefTbody,
        MockChefTr,
        MockChefTh,
        MockChefTd,
        RouterTestingModule,
        ChefPipesModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      providers: [
        FeatureFlagsService,
        ProjectService
      ]
    }).compileComponents();
  }));

  beforeEach(() => {
    jasmine.addMatchers(customMatchers);
    projectService = TestBed.inject(ProjectService);
    fixture = TestBed.createComponent(ProcessProgressBarComponent);
    component = fixture.componentInstance;
    store = TestBed.inject(Store);
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
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
      component.projects.applyRulesStart(); // start the update
      component.openConfirmUpdateStopModal();
      component.confirmApplyStop(); // emit the confirmation
    });

    it('keeps the modal open', () => {

      expect(component.confirmApplyStopModalVisible).toEqual(true);
    });

    it('has the projectService stop the rule updates', () => {
      expect(projectService.applyRulesStop).toHaveBeenCalled();
    });

  });

  describe('displays the percentage of completion', () => {
    it('labels the button with percentage during an update', () => {
      component.projects.applyRulesStart(); // start the update
      store.dispatch(new GetApplyRulesStatusSuccess( // side effect of the update
        genState(ApplyRulesStatusState.Running)));
      expect(component.percentageComplete).toEqual(50);
    });
  });

});

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
