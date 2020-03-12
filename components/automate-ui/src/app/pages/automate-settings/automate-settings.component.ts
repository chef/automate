import { Component, OnInit } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { FormGroup, FormBuilder } from '@angular/forms';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from '../../ngrx.reducers';
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

@Component({
  templateUrl: './automate-settings.component.html',
  styleUrls: ['./automate-settings.component.scss']
})

export class AutomateSettingsComponent implements OnInit {

  private defaultFormData2 = {
    eventFeedRemoveData: {
      category: 'event_feed',
      unit: { value: 'd', disabled: false },
      threshold: {value: '30', disabled: false},
      disabled: false
    },
    eventFeedServerActions: {
      category: 'infra',
      unit: { value: 'd', disabled: false },
      threshold: { value: '30', disabled: false },
      disabled: false
    },
    serviceGroupNoHealthChecks: {
      category: 'services',
      name: '',
      unit: { value: 'm', disabled: true },
      threshold: { value: '5', disabled: true },
      disabled: true
    },
    serviceGroupRemoveServices: {
      category: 'services',
      name: '',
      unit: { value: 'd', disabled: false },
      threshold: { value: '5', disabled: false },
      disabled: false
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
      unit: { value: 'd', disabled: false },
      threshold: { value: '30', disabled: false },
      disabled: false
    },
    complianceRemoveReports: {
      category: 'compliance',
      name: 'periodic_purge',
      unit: { value: 'd', disabled: false },
      threshold: { value: '30', disabled: false },
      disabled: false
    },
    complianceRemoveScans: {
      category: 'compliance',
      name: 'periodic_purge',
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

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private fb: FormBuilder,
    private telemetryService: TelemetryService
  ) {
    const formDetails = this.defaultFormData2;

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
    this.store.select(automateSettingsState)
      .subscribe((automateSettingsSelector) => {
        if (automateSettingsSelector.errorResp !== null) {
          const error = automateSettingsSelector.errorResp;
          const errMsg = 'Unable to load settings.';
          this.showErrorNotification(error, errMsg);
        } else {
          this.jobSchedulerStatus = automateSettingsSelector.jobSchedulerStatus;
          this.telemetryService.track('lifecycleConfiguration', this.jobSchedulerStatus);
          this.updateForm(this.jobSchedulerStatus);
          this.onChanges();
        }
      });
  }

  testPrintValues() {
    console.log(this.automateSettingsForm.value);
  }

  // Has the form changed?
  public noChanges() {
    return !this.formChanged;
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
      if (checked === true) {
        form.get(control).enable();
      } else {
        form.get(control).disable();
      }
    });
  }

  // Apply the changes that the user updated in the forms
  public applyChanges() {
    // Note: Services are currently not enabled through the form
    const jobs: IngestJob[] = [
      // March 10 - this will need full updating to match RespJob and IngestJob
      // IngestJobs.MissingNodes,
      // IngestJobs.MissingNodesForDeletion

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
      const job = new IngestJob(null, null);
      job.name = jobForm.name;
      job.category = jobForm.category;
      job.disabled = jobForm.disabled;
      job.threshold = jobForm.threshold + jobForm.unit;
      return job;
    });

    this.store.dispatch(new ConfigureSettings({jobs: jobs}));
    this.store.select(changeConfiguration)
      .subscribe((changeConfigurationSelector) => {
        if (changeConfigurationSelector.errorResp !== null) {
          const error = changeConfigurationSelector.errorResp;
          const errMsg = 'Unable to update one or more settings.';
          this.showErrorNotification(error, errMsg);
        } else {
          this.formChanged = false;
          this.showSuccessNotification();
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
  private onChanges() {
    this.automateSettingsForm.valueChanges
      .subscribe(_change => this.formChanged = true);
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

    console.log('jobScheduleStatus');
    console.log(jobSchedulerStatus);

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

  private getJobForm(job: string) {
    switch (job) {
      case IngestJobs.EventFeedRemoveData: {
        return this.automateSettingsForm.value['eventFeedRemoveData'];
      }
      case IngestJobs.EventFeedServerActions: {
        return this.automateSettingsForm.value['eventFeedServerActions'];
      }
      case IngestJobs.ServiceGroupNoHealthChecks: {
        return this.automateSettingsForm.value['serviceGroupNoHealthChecks'];
      }
      case IngestJobs.ServiceGroupRemoveServices: {
        return this.automateSettingsForm.value['serviceGroupRemoveServices'];
      }
      case IngestJobs.ClientRunsRemoveData: {
        return this.automateSettingsForm.value['clientRunsRemoveData'];
      }
      case IngestJobs.ClientRunsLabelMissing: {
        return this.automateSettingsForm.value['clientRunsLabelMissing'];
      }
      case IngestJobs.ClientRunsRemoveNodes: {
        return this.automateSettingsForm.value['clientRunsRemoveNodes'];
      }
      case IngestJobs.ComplianceRemoveReports: {
        return this.automateSettingsForm.value['complianceRemoveReports'];
      }
      case IngestJobs.ComplianceRemoveScans: {
        return this.automateSettingsForm.value['complianceRemoveScans'];
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
    const form = {
      unit: formUnit.toString(),
      threshold: formThreshold.toString(),
      disabled: job.disabled
    }

    switch (job.name) {
      case 'missing_nodes': {
        this.handleDisable(this.clientRunsRemoveData);
        [formThreshold, formUnit] = this.splitThreshold(job.threshold);
        this.clientRunsRemoveData.patchValue(form);
      }
      break;

      case 'missing_nodes_for_deletion': {
        this.handleDisable(this.clientRunsLabelMissing);
        [formThreshold, formUnit] = this.splitThreshold(job.threshold);
        this.clientRunsLabelMissing.patchValue(form);
      }
      break;

      case 'delete_nodes': {
        console.log('delete_nodes not implemented');
      }
      break;

      case 'periodic_purge_timeseries': {
        const formValues = job.purge_policies.elasticsearch;
        this.handleDisable(this.clientRunsRemoveNodes, formValues[0].disabled);
        this.clientRunsRemoveNodes.patchValue({
          threshold: formValues[0].older_than_days.toString(),
          disabled: formValues[0].disabled
        });
        // this maps to the EventFeed Chef Server Actions, though it lives in infra
        this.handleDisable(this.eventFeedServerActions, formValues[1].disabled);
        this.eventFeedServerActions.patchValue({
          threshold: formValues[1].older_than_days.toString(),
          disabled: formValues[1].disabled
        });
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

      switch (_job.name) {

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
        }
        break;

        default:
          break;
      }
    });
  }

  private handleDisable(form, disabled: boolean = false): void {
    if ( form.disabled || disabled ) {
      // this loops through the rest of the form to disable it
      const pertinentGroups = ['unit', 'threshold'];
      pertinentGroups.forEach(control => {
          form.get(control).disable();
      });
      return;
    }
  }

}
