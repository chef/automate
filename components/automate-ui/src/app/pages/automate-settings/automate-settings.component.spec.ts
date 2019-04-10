import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { AutomateSettingsComponent } from './automate-settings.component';
import { StoreModule } from '@ngrx/store';
import {
  automateSettingsEntityReducer
} from 'app/entities/automate-settings/automate-settings.reducer';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { FormGroup, FormBuilder } from '@angular/forms';
import {
  IngestJob,
  IngestJobs,
  JobSchedulerStatus
} from 'app/entities/automate-settings/automate-settings.model';

import { TelemetryService } from '../../services/telemetry/telemetry.service';

let mockJobSchedulerStatus: JobSchedulerStatus = null;

class MockTelemetryService {
  track() { }
}

describe('AutomateSettingsComponent', () => {
  let component: AutomateSettingsComponent;
  let fixture: ComponentFixture<AutomateSettingsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule,
        StoreModule.forRoot({
          automateSettings: automateSettingsEntityReducer
        })
      ],
      declarations: [
        AutomateSettingsComponent
      ],
      providers: [
        FormBuilder,
        { provide: TelemetryService, useClass: MockTelemetryService }
      ],
      schemas: [
        CUSTOM_ELEMENTS_SCHEMA
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(AutomateSettingsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  afterEach(() => {
    fixture.destroy();
  });

  it('exists', () => {
    expect(component).toBeTruthy();
  });

  it('sets defaults for all form groups', () => {
    expect(component.automateSettingsForm).not.toEqual(null);
    expect(component.automateSettingsForm instanceof FormGroup).toBe(true);
    expect(Object.keys(component.automateSettingsForm.controls)).toEqual([
      'eventFeed',
      'clientRuns',
      'complianceData',
      'missingNodes',
      'deleteMissingNodes'
    ]);
  });

  describe('patchDisableValue(form, value)', () => {
    it('updates the value of the disable control from the provided form', () => {
      expect(component.clientRunsForm.value.disable).toEqual(false);
      component.patchDisableValue(component.clientRunsForm, true);
      expect(component.clientRunsForm.value.disable).toEqual(true);
    });
  });

  describe('when jobSchedulerStatus is null', () => {
    it('does not update the forms', () => {
      const formBeforeUpdate = component.automateSettingsForm;
      component.updateForm(null);
      expect(component.automateSettingsForm).toEqual(formBeforeUpdate);
    });
  });

  describe('noChanges()', () => {
    it('reports if there has been any changes to the form', () => {
      component.ngOnInit();
      expect(component.noChanges()).toEqual(true);
      component.patchDisableValue(component.deleteMissingNodesForm, true);
      expect(component.noChanges()).toEqual(false);
    });
  });

  describe('when jobSchedulerStatus is set', () => {
    beforeAll(() => {
      const jobMissingNodes: IngestJob = {
        running: false,
        name: IngestJobs.MissingNodes,
        threshold: '60m',
        every: '1h'
      };
      const jobMissingNodesForDeletion: IngestJob = {
        running: true,
        name: IngestJobs.MissingNodesForDeletion,
        threshold: '24h',
        every: '60m'
      };
      mockJobSchedulerStatus = new JobSchedulerStatus(true, [
        jobMissingNodes,
        jobMissingNodesForDeletion
      ]);
    });

    it('updates the "missingNodes" form group correctly', () => {
      component.updateForm(mockJobSchedulerStatus);

      const missingNodesValues = component.automateSettingsForm
        .controls.missingNodes.value;

      expect(missingNodesValues.disable).toEqual(true);
      expect(missingNodesValues.threshold).toEqual('60');
      expect(missingNodesValues.unit).toEqual('m');
    });

    it('updates the "deleteMissingNodes" form group correctly', () => {
      component.updateForm(mockJobSchedulerStatus);

      const deleteMssingNodesValues = component.automateSettingsForm
        .controls.deleteMissingNodes.value;

      expect(deleteMssingNodesValues.disable).toEqual(false);
      expect(deleteMssingNodesValues.threshold).toEqual('24');
      expect(deleteMssingNodesValues.unit).toEqual('h');
    });

    it('does not updates the "eventFeed" form group', () => {
      component.updateForm(mockJobSchedulerStatus);

      const eventFeedValues = component.automateSettingsForm
        .controls.eventFeed.value;

      // These are the defaults
      expect(eventFeedValues.disable).toEqual(false);
      expect(eventFeedValues.threshold).toEqual('');
      expect(eventFeedValues.unit).toEqual('d');
    });

    describe('when user applyChanges()', () => {
      it('saves settings', () => {
        component.updateForm(mockJobSchedulerStatus);
        component.applyChanges();
        expect(component.formChanged).toEqual(false);
        expect(component.notificationType).toEqual('info');
        expect(component.notificationMessage)
          .toEqual('All settings have been updated successfully');
        expect(component.notificationVisible).toEqual(true);
      });

      xdescribe('and there is an error', () => {
        it('triggers a notification error (shows a banner)', () => {
          component.updateForm(mockJobSchedulerStatus);
          component.applyChanges();
          expect(component.notificationType).toEqual('error');
          expect(component.notificationMessage)
            .toEqual('Unable to update one or more settings. Verify the console logs.');
          expect(component.notificationVisible).toEqual(true);
        });
      });
    });
  });
});
