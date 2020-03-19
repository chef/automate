import { Component, OnInit, OnDestroy } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from '../../ngrx.reducers';
import { Subject } from 'rxjs';
import { distinctUntilKeyChanged, takeUntil } from 'rxjs/operators';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import {
  automateSettingsState,
  changeConfiguration
} from '../../entities/automate-settings/automate-settings.selectors';
import {
  GetSettings,
  ConfigureSettings
} from '../../entities/automate-settings/automate-settings.actions';
import {
  JobSchedulerStatus,
  IngestJob,
  IngestJobs
} from '../../entities/automate-settings/automate-settings.model';
import { TelemetryService } from '../../services/telemetry/telemetry.service';
import { Regex } from 'app/helpers/auth/regex';

@Component({
  templateUrl: './automate-settings.component.html',
  styleUrls: ['./automate-settings.component.scss']
})

export class AutomateSettingsComponent implements OnInit, OnDestroy {

  private defaultFormData = {
    eventFeedRemoveData: {
      category: 'event_feed',
      name: 'periodic_purge',
      nested_name: 'feed',
      unit: { value: 'd', disabled: false },
      threshold: [{ value: '30', disabled: false }, Validators.min(1)],
      disabled: false
    },
    eventFeedServerActions: {
      category: 'infra',
      name: 'periodic_purge_timeseries',
      nested_name: 'actions',
      unit: { value: 'd', disabled: false },
      threshold: { value: '30', disabled: false },
      disabled: false
    },
    serviceGroupNoHealthChecks: {
      category: 'services',
      name: '',
      unit: { value: 'm', disabled: true},
      threshold: { value: '5', disabled: true },
      disabled: false // special case: only alterable by the API so we want to show as enabled
    },
    serviceGroupRemoveServices: {
      category: 'services',
      name: '',
      unit: { value: 'd', disabled: true },
      threshold: { value: '5', disabled: true },
      disabled: false // special case: API not ready to alter
    },
    clientRunsRemoveData: {
      category: 'infra',
      name: 'missing_nodes',
      unit: { value: 'd', disabled: false },
      threshold: { value: '30', disabled: false },
      disabled: false
    },
    clientRunsLabelMissing: {
      category: 'infra',
      name: 'missing_nodes_for_deletion',
      unit: { value: 'd', disabled: false },
      threshold: { value: '30', disabled: false },
      disabled: false
    },
    clientRunsRemoveNodes: {
      category: 'infra',
      name: 'periodic_purge_timeseries',
      nested_name: 'converge-history',
      unit: { value: 'd', disabled: false },
      threshold: { value: '30', disabled: false },
      disabled: false
    },
    complianceRemoveReports: {
      category: 'compliance',
      name: 'periodic_purge',
      nested_name: 'compliance-reports',
      unit: { value: 'd', disabled: false },
      threshold: { value: '30', disabled: false },
      disabled: false
    },
    complianceRemoveScans: {
      category: 'compliance',
      name: 'periodic_purge',
      nested_name: 'compliance-scans',
      unit: { value: 'd', disabled: false },
      threshold: { value: '30', disabled: false },
      disabled: false
    }
  };


  // Event Feed
  eventFeedRemoveData: FormGroup;
  eventFeedServerActions: FormGroup;

  // Service Groups
  serviceGroupNoHealthChecks: FormGroup;
  serviceGroupRemoveServices: FormGroup;

  // Client Runs
  clientRunsRemoveData: FormGroup;
  clientRunsLabelMissing: FormGroup;
  clientRunsRemoveNodes: FormGroup;

  // Compliance
  complianceRemoveReports: FormGroup;
  complianceRemoveScans: FormGroup;

  automateSettingsForm: FormGroup;
  jobSchedulerStatus: JobSchedulerStatus;

  // Has the form changed?
  formChanged = false;

  // Notification bits
  notificationVisible = false;
  notificationType = 'info';
  notificationMessage = 'All settings have been updated successfully';

  saving = false;
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private fb: FormBuilder,
    private telemetryService: TelemetryService
  ) {
    const formDetails = this.defaultFormData;

//  EventFeed
    this.eventFeedRemoveData = this.fb.group(formDetails['eventFeedRemoveData']);
    this.eventFeedServerActions = this.fb.group(formDetails['eventFeedServerActions']);

    // Service Groups
    this.serviceGroupNoHealthChecks = this.fb.group(formDetails['serviceGroupNoHealthChecks']);
    this.serviceGroupRemoveServices = this.fb.group(formDetails['serviceGroupRemoveServices']);

    // Client Runs
    this.clientRunsRemoveData = this.fb.group(formDetails['clientRunsRemoveData']);
    this.clientRunsLabelMissing = this.fb.group(formDetails['clientRunsLabelMissing']);
    this.clientRunsRemoveNodes = this.fb.group(formDetails['clientRunsRemoveNodes']);

    // Compliance
    this.complianceRemoveReports = this.fb.group(formDetails['complianceRemoveReports']);
    this.complianceRemoveScans = this.fb.group(formDetails['complianceRemoveScans']);

    // Put the whole form together
    this.automateSettingsForm = this.fb.group({
      // Event Feed
      eventFeedRemoveData: this.eventFeedRemoveData,
      eventFeedServerActions: this.eventFeedServerActions,
      // Service Groups
      serviceGroupNoHealthChecks: this.serviceGroupNoHealthChecks,
      serviceGroupRemoveServices: this.serviceGroupRemoveServices,

      // Client Runs
      clientRunsRemoveData: this.clientRunsRemoveData,
      clientRunsLabelMissing: this.clientRunsLabelMissing,
      clientRunsRemoveNodes: this.clientRunsRemoveNodes,

      // Compliance
      complianceRemoveReports: this.complianceRemoveReports,
      complianceRemoveScans: this.complianceRemoveScans
    });
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.store.dispatch(new GetSettings({}));

    this.store.select(automateSettingsState).pipe(
        takeUntil(this.isDestroyed),
        distinctUntilKeyChanged('jobSchedulerStatus')
        )
      .subscribe((automateSettingsSelector) => {
        if (automateSettingsSelector.errorResp !== null) {
          const error = automateSettingsSelector.errorResp;
          const errMsg = 'Unable to load settings.';
          this.showErrorNotification(error, errMsg);
        } else {
          this.jobSchedulerStatus = automateSettingsSelector.jobSchedulerStatus;
          this.telemetryService.track('lifecycleConfiguration', this.jobSchedulerStatus);
          this.updateForm(this.jobSchedulerStatus);
        }
      });

  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  printValues() {
    console.log(this.eventFeedRemoveData);
    console.log(this.eventFeedServerActions);
  }

  // This prevents a user from being allowed to enter negative numbers
  // or other actions that we dont want to allow
  public preventNegatives(key: string) {
    const allowedKeys = [
      '1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
      'Backspace', 'ArrowLeft', 'ArrowRight'
    ];
    if ( !allowedKeys.includes(key) ) {
      return false;
    }
  }


  public toggleInput(form, checked: boolean) {
    // patchValue is a workaround for the chef-checkbox because we need to be
    // able to store a reference to it being checked or not
    form.patchValue({
      disabled: !checked
    });

    // this loops through the rest of the form to disable it
    const pertinentGroups = [ 'unit', 'threshold' ];
    pertinentGroups.forEach(control => {
      if ( checked ) {
        form.get(control).enable();

        if (control === 'threshold') {
          form.controls[control].setValidators([
            Validators.pattern(Regex.patterns.POSITIVE_INT)]); // set validators here
            form.controls[control].updateValueAndValidity();
        }

      } else {
          form.get(control).disable();
      }
    });
  }

  // Apply the changes that the user updated in the forms
  public applyChanges() {
    this.saving = true;
    // Note: Services are currently not enabled through the form
    const jobs: IngestJob[] = [
      // Event Feed
      IngestJobs.EventFeedRemoveData,
      IngestJobs.EventFeedServerActions,
      // Service Groups
      IngestJobs.ServiceGroupNoHealthChecks,
      IngestJobs.ServiceGroupRemoveServices,
      // Client Runs
      IngestJobs.ClientRunsRemoveData,
      IngestJobs.ClientRunsLabelMissing,
      IngestJobs.ClientRunsRemoveNodes,
      // Compliance
      IngestJobs.ComplianceRemoveReports,
      IngestJobs.ComplianceRemoveScans
    ].map(jobName => {
      const jobForm = this.getJobForm(jobName);
      const isNested = jobForm.nested_name ? true : false;
      const job = new IngestJob(null, null);
      job.category = jobForm.category;
      job.name = jobForm.name;
      job.disabled = jobForm.disabled;
      job.threshold = jobForm.threshold + jobForm.unit;
      if ( isNested ) {
        job.nested_name = jobForm.nested_name;
        job.threshold = jobForm.threshold;
      }

      return job;
    });


    this.store.dispatch(new ConfigureSettings({jobs: jobs}));
    this.store.select(changeConfiguration).pipe(takeUntil(this.isDestroyed))
      .subscribe((changeConfigurationSelector) => {
        if (changeConfigurationSelector.errorResp !== null) {
          const error = changeConfigurationSelector.errorResp;
          const errMsg = 'Unable to update one or more settings.';
          this.showErrorNotification(error, errMsg);
          this.store.dispatch(new GetSettings({})); // reset form to previously stored settings
          this.saving = false;
        } else if (changeConfigurationSelector.status === 'loadingSuccess') {
          this.formChanged = false;
          this.showSuccessNotification();
          this.saving = false;
        }
      });
  }

  // Hides the notification banner
  public hideNotification() {
    this.notificationVisible = false;
  }

  // Returns timeout depending on the type of notification is being displayed
  public notificationTimeout() {
    switch (this.notificationType) {
      case 'error':
        return 8;
      default:
        return 4;
    }
  }

  // Subscribes to any change inside the automateSettingsForm
  private onChanges(): void {
    this.automateSettingsForm.valueChanges.pipe(takeUntil(this.isDestroyed))
      .subscribe(_change => {
        this.formChanged = true;
        // Loop through forms and check for validity, then set to true or false
      });
  }

  private showErrorNotification(error: HttpErrorResponse, msg: string) {
    // Extract the error message from the HttpErrorResponse
    // if it is available inside the body.
    //
    // The format looks similar like:
    // {
    //   "error":"invalid time unit: '-1d'",
    //   "code":3,
    //   "details":[]
    // }
    if (error.error.error) {
      msg = msg + ' [' + error.error.error + ']';
    }
    console.error(error.message);
    this.notificationType = 'error';
    this.notificationMessage = msg;
    this.notificationVisible = true;
  }

  private showSuccessNotification() {
    this.notificationType = 'info';
    this.notificationMessage = 'All settings have been updated successfully';
    this.notificationVisible = true;
  }

  // Update forms until we get the job scheduler status
  public updateForm(jobSchedulerStatus: JobSchedulerStatus) {

    if (jobSchedulerStatus === null) {
      return;
    }

    jobSchedulerStatus.jobs.forEach((job: IngestJob) => {

      switch (job.category) {
        case 'infra': {
          this.populateInfra(job);
        }
        break;

        case 'compliance': // fallthrough
        case 'event_feed': {
          this.populateNested(job);
        }
        break;

        default:
          break;
      }
    });

    // Subscribe to new changes on the form after the new values have been populated
    this.onChanges();
  }

  private getJobForm(jobName: string) {
    switch (jobName) {
      case IngestJobs.EventFeedRemoveData: {
        return this.automateSettingsForm.getRawValue()['eventFeedRemoveData'];
      }
      case IngestJobs.EventFeedServerActions: {
        return this.automateSettingsForm.getRawValue()['eventFeedServerActions'];
      }
      case IngestJobs.ServiceGroupNoHealthChecks: {
        return this.automateSettingsForm.getRawValue()['serviceGroupNoHealthChecks'];
      }
      case IngestJobs.ServiceGroupRemoveServices: {
        return this.automateSettingsForm.getRawValue()['serviceGroupRemoveServices'];
      }
      case IngestJobs.ClientRunsRemoveData: {
        return this.automateSettingsForm.getRawValue()['clientRunsRemoveData'];
      }
      case IngestJobs.ClientRunsLabelMissing: {
        return this.automateSettingsForm.getRawValue()['clientRunsLabelMissing'];
      }
      case IngestJobs.ClientRunsRemoveNodes: {
        return this.automateSettingsForm.getRawValue()['clientRunsRemoveNodes'];
      }
      case IngestJobs.ComplianceRemoveReports: {
        return this.automateSettingsForm.getRawValue()['complianceRemoveReports'];
      }
      case IngestJobs.ComplianceRemoveScans: {
        return this.automateSettingsForm.getRawValue()['complianceRemoveScans'];
      }
    }
  }

  private splitThreshold(threshold: string) {
    return [
      threshold.slice(0, threshold.length - 1),
      threshold.slice(-1)
    ];
  }

  private populateInfra(job: IngestJob): void {
    let formThreshold, formUnit;

    switch (job.name) { // strongly type this so that it throws error
      case 'missing_nodes': {
        this.handleDisable(this.clientRunsRemoveData, job.disabled);
        [formThreshold, formUnit] = this.splitThreshold(job.threshold);
        this.clientRunsRemoveData.patchValue({
          unit: formUnit,
          threshold: formThreshold,
          disabled: job.disabled
        });
      }
      break;

      case 'missing_nodes_for_deletion': {
        this.handleDisable(this.clientRunsLabelMissing, job.disabled);
        [formThreshold, formUnit] = this.splitThreshold(job.threshold);
        this.clientRunsLabelMissing.patchValue({
          unit: formUnit,
          threshold: formThreshold,
          disabled: job.disabled
        });
      }
      break;

      case 'delete_nodes': {
        // delete_nodes not implemented
      }
      break;

      case 'periodic_purge_timeseries': {
        this.populateNested(job);
      }
      break;

      default:
        break;
    }
  }

  private populateNested(job: IngestJob): void {
    const _jobs = job.purge_policies.elasticsearch;

    _jobs.forEach(_job => {
      const form = {
        threshold: _job.older_than_days,
        disabled: _job.disabled
      };

      switch (_job.name) { // strongly type this so that it throws error

        case 'compliance-reports': {
          this.handleDisable(this.complianceRemoveReports, _job.disabled);
          this.complianceRemoveReports.patchValue(form);
        }
        break;

        case 'compliance-scans': {
          this.handleDisable(this.complianceRemoveScans, _job.disabled);
          this.complianceRemoveScans.patchValue(form);
        }
        break;

        case 'feed': {
          this.handleDisable(this.eventFeedRemoveData, _job.disabled);
          this.eventFeedRemoveData.patchValue(form);

          // Set Validations Example
          this.eventFeedRemoveData.controls['threshold'].setValidators([
            Validators.pattern(Regex.patterns.POSITIVE_INT)]);
          this.eventFeedRemoveData.controls['threshold'].updateValueAndValidity();
        }
        break;

        case 'actions': {
          this.handleDisable(this.eventFeedServerActions, _job.disabled);
          this.eventFeedServerActions.patchValue(form);
        }
        break;

        case 'converge-history': {
          this.handleDisable(this.clientRunsRemoveNodes, _job.disabled);
          this.clientRunsRemoveNodes.patchValue(form);
        }
          break;

        default:
          break;
      }
    });
  }

  private handleDisable(form, disabled: boolean = false): void {
    // this loops through the rest of the form to disable/enable it
    const pertinentGroups = ['unit', 'threshold'];
    pertinentGroups.forEach(control => {
      if ( disabled ) {
        form.get(control).disable();
      } else {
        form.get(control).enable();
      }
    });
    return;
  }

}
