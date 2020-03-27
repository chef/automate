import { Component, OnInit, OnDestroy } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { FormGroup, FormBuilder } from '@angular/forms';
import { Store, select } from '@ngrx/store';
import { NgrxStateAtom } from '../../ngrx.reducers';
import { Subject } from 'rxjs';
import { distinctUntilKeyChanged, takeUntil, filter } from 'rxjs/operators';
import { pendingState } from 'app/entities/entities';
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
  IngestJobs,
  InfraJobName,
  NestedJobName,
  DefaultFormData,
  JobCategories
} from '../../entities/automate-settings/automate-settings.model';
import { TelemetryService } from '../../services/telemetry/telemetry.service';

@Component({
  templateUrl: './automate-settings.component.html',
  styleUrls: ['./automate-settings.component.scss']
})

export class AutomateSettingsComponent implements OnInit, OnDestroy {

  private defaultFormData: DefaultFormData = {
    eventFeedRemoveData: {
      category: JobCategories.EventFeed,
      name: 'periodic_purge',
      nested_name: NestedJobName.Feed,
      unit: { value: 'd', disabled: false },
      threshold: { value: '30', disabled: false },
      disabled: false
    },
    eventFeedServerActions: {
      category: JobCategories.Infra,
      name: 'periodic_purge_timeseries',
      nested_name: NestedJobName.Actions,
      unit: { value: 'd', disabled: false },
      threshold: { value: '30', disabled: false },
      disabled: false
    },
    serviceGroupNoHealthChecks: {
      category: JobCategories.Services,
      name: '',
      unit: { value: 'm', disabled: true},
      threshold: { value: '5', disabled: true },
      disabled: false // special case: only alterable by the API so we want to show as enabled
    },
    serviceGroupRemoveServices: {
      category: JobCategories.Services,
      name: '',
      unit: { value: 'd', disabled: true },
      threshold: { value: '5', disabled: true },
      disabled: false // special case: API not ready to alter
    },
    clientRunsRemoveData: {
      category: JobCategories.Infra,
      name: 'missing_nodes',
      unit: { value: 'd', disabled: false },
      threshold: { value: '30', disabled: false },
      disabled: false
    },
    clientRunsLabelMissing: {
      category: JobCategories.Infra,
      name: 'missing_nodes_for_deletion',
      unit: { value: 'd', disabled: false },
      threshold: { value: '30', disabled: false },
      disabled: false
    },
    clientRunsRemoveNodes: {
      category: JobCategories.Infra,
      name: 'periodic_purge_timeseries',
      nested_name: NestedJobName.ConvergeHistory,
      unit: { value: 'd', disabled: false },
      threshold: { value: '30', disabled: false },
      disabled: false
    },
    complianceRemoveReports: {
      category: JobCategories.Compliance,
      name: 'periodic_purge',
      nested_name: NestedJobName.ComplianceReports,
      unit: { value: 'd', disabled: false },
      threshold: { value: '30', disabled: false },
      disabled: false
    },
    complianceRemoveScans: {
      category: JobCategories.Compliance,
      name: 'periodic_purge',
      nested_name: NestedJobName.ComplianceScans,
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

  // Are settings currently saving
  saving = false;

  // Notification bits
  notificationVisible = false;
  notificationType = 'info';
  notificationMessage = 'All settings have been updated successfully';

  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private fb: FormBuilder,
    private telemetryService: TelemetryService
  ) {
    const formDetails = this.defaultFormData;

//  EventFeed
    this.eventFeedRemoveData = this.fb.group(formDetails.eventFeedRemoveData);
    this.eventFeedServerActions = this.fb.group(formDetails.eventFeedServerActions);

    // Service Groups
    this.serviceGroupNoHealthChecks = this.fb.group(formDetails.serviceGroupNoHealthChecks);
    this.serviceGroupRemoveServices = this.fb.group(formDetails.serviceGroupRemoveServices);

    // Client Runs
    this.clientRunsRemoveData = this.fb.group(formDetails.clientRunsRemoveData);
    this.clientRunsLabelMissing = this.fb.group(formDetails.clientRunsLabelMissing);
    this.clientRunsRemoveNodes = this.fb.group(formDetails.clientRunsRemoveNodes);

    // Compliance
    this.complianceRemoveReports = this.fb.group(formDetails.complianceRemoveReports);
    this.complianceRemoveScans = this.fb.group(formDetails.complianceRemoveScans);

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

      // subscribe to changeConfiguration
      this.store.pipe(
        select(changeConfiguration),
        filter(change => this.saving && !pendingState(change)),
        takeUntil(this.isDestroyed)
        )
        .subscribe((changeConfigurationSelector) => {
          if (changeConfigurationSelector.errorResp !== null) {
            const error = changeConfigurationSelector.errorResp;
            const errMsg = 'Unable to update one or more settings.';
            this.showErrorNotification(error, errMsg);
            this.store.dispatch(new GetSettings({})); // reset form to previously stored settings
            this.automateSettingsForm.markAsPristine();
            this.saving = false;
          } else if (changeConfigurationSelector.status === 'loadingSuccess') {
            this.showSuccessNotification();
            this.automateSettingsForm.markAsPristine();
            this.saving = false;
          }
        });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  // This prevents a user from being allowed to enter negative numbers
  // or other actions that we dont want to allow
  public preventNegatives(key: string) {
    const allowedKeys = [
      '1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
      'Backspace', 'ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown', 'Tab'
    ];
    return allowedKeys.includes(key);
  }

  public handleFormActivation(form, checked: boolean): void {
    // patchValue does not mark the form dirty, so we need to do it manually;
    form.get('disabled').markAsDirty();
    // patchValue is a workaround for the chef-checkbox because we need to be
    // able to store a reference to it being checked or not
    form.patchValue({
      disabled: !checked
    });

    // this loops through the rest of the form to disable the relevant controls;
    const pertinentGroups = [ 'unit', 'threshold' ];
    pertinentGroups.forEach(control => {
      if ( checked ) {
        form.get(control).enable();
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

      // If the user doesn't enter any number at all - this defaults to 0
      if (jobForm.threshold === null) {
        job.threshold = '0' + jobForm.unit;
      } else {
        job.threshold = jobForm.threshold + jobForm.unit;
      }

      if ( isNested ) {
        job.nested_name = jobForm.nested_name;
        job.threshold = jobForm.threshold;  // Automatically becomes a 0 from
                                            // parseInt in reqeust if left blank
      }

      return job;
    });

    this.store.dispatch(new ConfigureSettings({jobs: jobs}));
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

    switch (job.name) {
      case InfraJobName.MissingNodes: {
        this.handleDisable(this.clientRunsRemoveData, job.disabled);
        [formThreshold, formUnit] = this.splitThreshold(job.threshold);
        this.clientRunsRemoveData.patchValue({
          unit: formUnit,
          threshold: formThreshold,
          disabled: job.disabled
        });
      }
      break;

      case InfraJobName.MissingNodesForDeletion: {
        this.handleDisable(this.clientRunsLabelMissing, job.disabled);
        [formThreshold, formUnit] = this.splitThreshold(job.threshold);
        this.clientRunsLabelMissing.patchValue({
          unit: formUnit,
          threshold: formThreshold,
          disabled: job.disabled
        });
      }
      break;

      case InfraJobName.DeleteNodes: {
        // delete_nodes not yet implemented
      }
      break;

      case InfraJobName.PeriodicPurgeTimeseries: {
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

      switch (_job.name) {
        case NestedJobName.ComplianceReports: {
          this.handleDisable(this.complianceRemoveReports, _job.disabled);
          this.complianceRemoveReports.patchValue(form);
        }
        break;

        case NestedJobName.ComplianceScans: {
          this.handleDisable(this.complianceRemoveScans, _job.disabled);
          this.complianceRemoveScans.patchValue(form);
        }
        break;

        case NestedJobName.Feed: {
          this.handleDisable(this.eventFeedRemoveData, _job.disabled);
          this.eventFeedRemoveData.patchValue(form);
        }
        break;

        case NestedJobName.Actions: {
          this.handleDisable(this.eventFeedServerActions, _job.disabled);
          this.eventFeedServerActions.patchValue(form);
        }
        break;

        case NestedJobName.ConvergeHistory: {
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
    // this loops through the rest of the form to disable/enable relevant controls
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
