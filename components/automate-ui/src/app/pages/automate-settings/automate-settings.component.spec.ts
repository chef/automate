import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { StoreModule } from '@ngrx/store';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { FormGroup, FormBuilder, FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatSelectModule } from '@angular/material/select';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';

import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import {
  IngestJob,
  JobSchedulerStatus,
  JobCategories
} from 'app/entities/automate-settings/automate-settings.model';

import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { TelemetryService } from '../../services/telemetry/telemetry.service';
import { AutomateSettingsComponent } from './automate-settings.component';

import { using } from 'app/testing/spec-helpers';

let mockJobSchedulerStatus: JobSchedulerStatus = null;

class MockTelemetryService {
  track() { }
}

// A reusable list of all the form names
const ALL_FORMS = [
  'eventFeedRemoveData',
  'eventFeedServerActions',
  'serviceGroupNoHealthChecks',
  'serviceGroupRemoveServices',
  'clientRunsRemoveData',
  'clientRunsLabelMissing',
  'clientRunsRemoveNodes',
  'complianceRemoveReports',
  'complianceRemoveScans'
];

describe('AutomateSettingsComponent', () => {
  let component: AutomateSettingsComponent;
  let fixture: ComponentFixture<AutomateSettingsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        FormsModule,
        ReactiveFormsModule,
        MatFormFieldModule,
        MatSelectModule,
        BrowserAnimationsModule,
        HttpClientTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      declarations: [
        AutomateSettingsComponent
      ],
      providers: [
        FormBuilder,
        FeatureFlagsService,
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
    expect(Object.keys(component.automateSettingsForm.controls)).toEqual(ALL_FORMS);
  });

  describe('toggleInput(form, value)', () => {

    using(ALL_FORMS
        // Service Groups on not currently uncheckable through the UI
        .filter( form => !['serviceGroupNoHealthChecks', 'serviceGroupRemoveServices']
        .includes(form)),
        function( form: string) {
      it(`deactivates the associated ${form} form`, () => {
        expect(component[form].value.disabled).toEqual(false);
        component.toggleInput(component[form], false);
        expect(component[form].value.disabled).toEqual(true);
        expect(component[form].get('unit').disabled).toBe(true);
        expect(component[form].get('threshold').disabled).toBe(true);
      });
    });

    using(ALL_FORMS
        // Service Groups on not currently uncheckable through the UI
        .filter( form => !['serviceGroupsNoHealthChecks', 'serviceGroupRemoveServices']
        .includes(form)),
        function (form: string) {
          it(`activates the associated ${form} form`, () => {
        component[form].patchValue({disabled: true}); // Deactivate form to start
        expect(component[form].value.disabled).toEqual(true);
        component.toggleInput(component[form], true);
        expect(component[form].value.disabled).toEqual(false);
        expect(component[form].get('unit').disabled).toBe(false);
        expect(component[form].get('threshold').disabled).toBe(false);
      });
    });

  });

  describe('when jobSchedulerStatus is null', () => {
    it('does not update the forms', () => {
      const formBeforeUpdate = component.automateSettingsForm;
      component.updateForm(null);
      expect(component.automateSettingsForm).toEqual(formBeforeUpdate);
    });
  });

  describe('when jobSchedulerStatus is set', () => {
    beforeAll(() => {
      const eventFeedRemoveData: IngestJob = {
        name: 'periodic_purge',
        category: JobCategories.EventFeed,
        disabled: true,
        threshold: '',
        purge_policies: {
          elasticsearch: [
            {
              name: 'feed',
              older_than_days: 53,
              disabled: false
            }
          ]
        }
      };

      const infraNestedForms: IngestJob = {
        name: 'periodic_purge_timeseries',
        category: JobCategories.Infra,
        disabled: false,
        threshold: '',
        purge_policies: {
          elasticsearch: [
            {
              name: 'actions',
              older_than_days: 22, // default is 30, since disabled
              disabled: true       // is true older than should be null
            },
            {
              name: 'converge-history',
              older_than_days: 12,
              disabled: false
            }
          ]
        }
      };

      const complianceForms: IngestJob = {
        category: JobCategories.Compliance,
        name: 'periodic_purge',
        threshold: '',
        disabled: true,
        purge_policies: {
          elasticsearch: [
            {
              name: 'compliance-reports',
              older_than_days: 105,
              disabled: false
            },
            {
              name: 'compliance-scans',
              older_than_days: 92,
              disabled: false
            }
          ]
        }
      };

      const clientRunsRemoveData: IngestJob = {
        category: JobCategories.Infra,
        name: 'missing_nodes',
        disabled : false,
        threshold : '7d'
      };

      const clientRunsLabelMissing: IngestJob = {
        category: JobCategories.Infra,
        name: 'missing_nodes_for_deletion',
        disabled: false,
        threshold: '14m'
      };

      mockJobSchedulerStatus = new JobSchedulerStatus([
        eventFeedRemoveData,
        infraNestedForms,
        clientRunsRemoveData,
        clientRunsLabelMissing,
        complianceForms
      ]);
    });

    function genInjestJob(category: string, name: string, threshold: string, disabled: boolean) {
      return { category, name, threshold, disabled };
    }


    function genNestedIngestJob(category: string, name: string, nested_name: string,
                                threshold: number, disabled: boolean) {
      return {
        name,
        category,
        purge_policies: {
          elasticsearch: [
            {
              name: nested_name,
              older_than_days: threshold,
              disabled
            }
          ]
        }
      };
    }

    using([
      // Event Feed
      ['eventFeedRemoveData', 'nested', 'feed',
          genNestedIngestJob('event_feed', 'periodic_purge', 'feed', 1, false)] ,
      ['eventFeedServerActions', 'nested', 'actions',
          genNestedIngestJob('infra', 'periodic_purge_timeseries', 'actions', 2, false)],

      // Services --> not yet enabled
      // ['serviceGroupNoHealthChecks'],
      // ['serviceGroupRemoveServices'],

      // Client Runs
      ['clientRunsRemoveData', 'non-nested', 'not applicable',
          genInjestJob('infra', 'missing_nodes', '5m', false)],
      ['clientRunsLabelMissing', 'non-nested', 'not applicable',
          genInjestJob('infra', 'missing_nodes_for_deletion', '6h', false)],
      ['clientRunsRemoveNodes', 'nested', 'converge-history',
          genNestedIngestJob('infra', 'periodic_purge_timeseries', 'converge-history', 7, false)],

        // Compliance
      ['complianceRemoveReports', 'nested', 'compliance-reports',
          genNestedIngestJob('compliance', 'periodic_purge', 'compliance-reports', 8, false)],
      ['complianceRemoveScans', 'nested', 'compliance-scans',
          genNestedIngestJob('compliance', 'periodic_purge', 'compliance-scans', 9, false)]
    ], function(formName: string, jobType: string, nestedName: string, job: IngestJob) {
      it(`when updating ${formName} form,
            the form data is extracted from the ${jobType} form`, () => {
        const thisJobScheduler = new JobSchedulerStatus([job]);
        component.updateForm(thisJobScheduler);

        const newFormValues = component[formName].value;

        let jobData = job;
        if (jobType === 'nested') {
          jobData = job.purge_policies.elasticsearch.find(item => item.name === nestedName);
          expect(newFormValues.threshold).toEqual(jobData.older_than_days);
        } else {
          // non-nested threshold is stored differently, so we need to separate it
          // into threshold and unit first.
          const [jobThreshold, jobUnit] = [job.threshold.slice(0, job.threshold.length - 1),
                                           job.threshold.slice(-1)
          ];
          expect(newFormValues.threshold).toEqual(jobThreshold);
          expect(newFormValues.unit).toEqual(jobUnit);
        }

        // all forms share the same expectation of being disabled
        expect(newFormValues.disabled).toEqual(jobData.disabled);
      });
    });

    describe('when user applyChanges()', () => {
      it('saves settings', () => {
        component.updateForm(mockJobSchedulerStatus);
        component.applyChanges();

        // expect(component.notificationVisible).toBe(true);
        expect(component.notificationType).toEqual('info');
        expect(component.notificationMessage)
        .toEqual('All settings have been updated successfully');
        expect(component.formChanged).toEqual(false);
        // expect(component.saving).toEqual(false);
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
