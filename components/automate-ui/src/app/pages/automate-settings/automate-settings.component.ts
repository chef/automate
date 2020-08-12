import { Component, OnInit, OnDestroy, ElementRef, ViewChild } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { FormGroup, FormBuilder, AbstractControl, Validators } from '@angular/forms';
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
  NonNestedJobName,
  NestedJobName,
  DefaultFormData,
  JobCategories
} from '../../entities/automate-settings/automate-settings.model';
import { TelemetryService } from '../../services/telemetry/telemetry.service';
import { ProductDeployedService } from 'app/services/product-deployed/product-deployed.service';

@Component({
  templateUrl: './automate-settings.component.html',
  styleUrls: ['./automate-settings.component.scss']
})

export class AutomateSettingsComponent implements OnInit, OnDestroy {
  @ViewChild('dataLifeCycleFormElement', {read: ElementRef}) dataLifeCycleFormElement: ElementRef;

  public isDesktopView = false;

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

  // Change origin boolean for formControl directive
  shouldResetValues = false;

  // Notification bits
  notificationVisible = false;
  notificationType = 'info';
  notificationMessage = 'Settings saved.';

  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private fb: FormBuilder,
    private telemetryService: TelemetryService,
    private productDeployedService: ProductDeployedService
  ) {

    this.isDesktopView = this.productDeployedService.isProductDeployed('desktop');

    const formDetails = this.getDefaultFormData(this.isDesktopView);

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
            this.saving = false;
            // After a successful save, trigger a notification to FormControlDirective
            // to consider the newly updated values as the new "original" values
            this.shouldResetValues = true;
            this.automateSettingsForm.markAsPristine();
          }
        });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  // This prevents a user from being allowed to enter negative numbers
  // or other actions that we don't want to allow
  public preventNegatives(key: string) {
    const allowedKeys = [
      '1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
      'Backspace', 'ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown', 'Tab'
    ];
    return allowedKeys.includes(key);
  }

  // Update the width of input when greater than one digit
  public autoUpdateInputWidth(element: HTMLInputElement): void {
    // Keep default in sync with input width in automate-settings.component.scss
    const DEFAULT_WIDTH_PIXELS = 64;
    const numDigits = element.value.length;

    if (numDigits > 1) {
      element.style.width = `${DEFAULT_WIDTH_PIXELS + (9 * (numDigits - 1))}px`;
    } else {
      element.style.width = `${DEFAULT_WIDTH_PIXELS}px`;
    }
  }

  private setEnabled(control: AbstractControl, enabled: boolean): void {
    if (enabled) {
      control.enable();
      control.setValidators([Validators.required]);
      control.updateValueAndValidity();
    } else {
      control.disable();
    }
  }

  public handleFormActivation(form: FormGroup, checked: boolean): void {
    // patchValue does not mark the form dirty, so we need to do it manually;
    form.get('disabled').markAsDirty();
    // patchValue is a workaround for the chef-checkbox because we need to be
    // able to store a reference to it being checked or not
    form.patchValue({
      disabled: !checked
    });


    // this disables the relevant controls;
    this.setEnabled(form.controls.unit, checked);
    this.setEnabled(form.controls.threshold, checked);
  }

  // Apply the changes that the user updated in the forms
  public applyChanges() {
    this.saving = true;
    this.shouldResetValues = false;
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
      // Extract the raw values from the formGroup so that
      // we can make sure to keep disabled inputs populated in the UI
      const jobForm = this[jobName].getRawValue();

      const isNested = jobForm.nested_name ? true : false;
      const job = new IngestJob(null, null);
      job.category = jobForm.category;
      job.name = jobForm.name;
      job.disabled = jobForm.disabled;

      // If the user doesn't enter any number at all - this defaults to 0
      jobForm.threshold === null
        // When not nested, threshold needs to be a string ex: '4w'
        ? job.threshold = '0' + jobForm.unit
        : job.threshold = jobForm.threshold + jobForm.unit;

      if ( isNested ) {
        // When nested, threshold needs to be a number ex: 16
        job.nested_name = jobForm.nested_name;
        job.threshold = jobForm.threshold;  // Automatically becomes a 0 from
                                            // parseInt in request if left blank
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
        case 'services':
        case 'infra': {
          this.populateNonNested(job);
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

    // Update all number inputs to grow if holding number larger than two digits
    const numberInputs = Array.from(
      this.dataLifeCycleFormElement.nativeElement.querySelectorAll('.auto-update-width'));
    numberInputs.forEach((element: HTMLInputElement) => this.autoUpdateInputWidth(element));

    // After a successful load of initial values, trigger a notification
    // to FormControlDirective to treat them as the "original" values.
    this.shouldResetValues = true;
    this.automateSettingsForm.markAsPristine();
  }

  // Splits a packed threshold into a number and a unit, where unit is a single character
  // example: '12d' => ['12', 'd']
  private splitThreshold(threshold: string): [string, string] {
    return [
      threshold.slice(0, threshold.length - 1),
      threshold.slice(-1)
    ];
  }

  private populateNonNested(job: IngestJob): void {
    let formThreshold, formUnit;

    switch (job.name) {
      case NonNestedJobName.MissingNodesForDeletion: {
        if (!this.isDesktopView) {
          this.handleDisable(this.clientRunsRemoveNodes, job.disabled);
        }

        [formThreshold, formUnit] = this.splitThreshold(job.threshold);
        this.clientRunsRemoveNodes.patchValue({
          unit: formUnit,
          threshold: formThreshold,
          disabled: job.disabled
        });
      }
      break;

      case NonNestedJobName.MissingNodes: {
        if (!this.isDesktopView) {
          this.handleDisable(this.clientRunsLabelMissing, job.disabled);
        }
        [formThreshold, formUnit] = this.splitThreshold(job.threshold);
        this.clientRunsLabelMissing.patchValue({
          unit: formUnit,
          threshold: formThreshold,
          disabled: job.disabled
        });
      }
      break;

      case NonNestedJobName.DeleteNodes: {
        // delete_nodes not yet implemented
      }
      break;

      case NonNestedJobName.PeriodicPurgeTimeseries: {
        this.populateNested(job);
      }
      break;

      case NonNestedJobName.DisconnectedServices:
        this.handleDisable(this.serviceGroupNoHealthChecks, job.disabled);
        [formThreshold, formUnit] = this.splitThreshold(job.threshold);
        this.serviceGroupNoHealthChecks.patchValue({
          unit: formUnit,
          threshold: formThreshold,
          disabled: job.disabled
        });
        break;

      case NonNestedJobName.DeleteDisconnectedServices:
        this.handleDisable(this.serviceGroupRemoveServices, job.disabled);
        [formThreshold, formUnit] = this.splitThreshold(job.threshold);
        this.serviceGroupRemoveServices.patchValue({
          unit: formUnit,
          threshold: formThreshold,
          disabled: job.disabled
        });
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
          this.handleDisable(this.clientRunsRemoveData, _job.disabled);
          this.clientRunsRemoveData.patchValue(form);
        }
          break;

        default:
          break;
      }
    });
  }

  private handleDisable(form: FormGroup, disabled: boolean = false): void {
    // this disables the relevant controls
    // We have to pass in !disabled because the function is initially build for enabling
    this.setEnabled(form.controls.unit, !disabled);
    this.setEnabled(form.controls.threshold, !disabled);
  }

  private getDefaultFormData(isDesktopView: boolean): DefaultFormData {
    return {
      eventFeedRemoveData: {
        category: JobCategories.EventFeed,
        name: NonNestedJobName.PeriodicPurge,
        nested_name: NestedJobName.Feed,
        unit: { value: 'd', disabled: false },
        threshold: [{ value: '30', disabled: false}, Validators.required],
        disabled: false
      },
      eventFeedServerActions: {
        category: JobCategories.Infra,
        name: NonNestedJobName.PeriodicPurgeTimeseries,
        nested_name: NestedJobName.Actions,
        unit: { value: 'd', disabled: false },
        threshold: [{ value: '30', disabled: false }, Validators.required],
        disabled: false
      },
      serviceGroupNoHealthChecks: {
        category: JobCategories.Services,
        name: NonNestedJobName.DisconnectedServices,
        unit: { value: 'm', disabled: false},
        threshold: [{ value: '5', disabled: false }, Validators.required],
        disabled: false
      },
      serviceGroupRemoveServices: {
        category: JobCategories.Services,
        name: NonNestedJobName.DeleteDisconnectedServices,
        unit: { value: 'd', disabled: false },
        threshold: [{ value: '5', disabled: false }, Validators.required],
        disabled: false
      },
      clientRunsRemoveData: {
        category: JobCategories.Infra,
        name: NonNestedJobName.PeriodicPurgeTimeseries,
        nested_name: NestedJobName.ConvergeHistory,
        unit: { value: 'd', disabled: false },
        threshold: [{ value: '30', disabled: false }, Validators.required],
        disabled: false
      },
      clientRunsLabelMissing: {
        category: JobCategories.Infra,
        name: NonNestedJobName.MissingNodes,
        unit: { value: 'd', disabled: isDesktopView },
        threshold: [{ value: '30', disabled: isDesktopView }, Validators.required],
        disabled: isDesktopView
      },
      clientRunsRemoveNodes: {
        category: JobCategories.Infra,
        name: NonNestedJobName.MissingNodesForDeletion,
        unit: { value: 'd', disabled: isDesktopView },
        threshold: [{ value: '30', disabled: isDesktopView }, Validators.required],
        disabled: isDesktopView
      },
      complianceRemoveReports: {
        category: JobCategories.Compliance,
        name: NonNestedJobName.PeriodicPurge,
        nested_name: NestedJobName.ComplianceReports,
        unit: { value: 'd', disabled: false },
        threshold: [{ value: '30', disabled: false }, Validators.required],
        disabled: false
      },
      complianceRemoveScans: {
        category: JobCategories.Compliance,
        name: NonNestedJobName.PeriodicPurge,
        nested_name: NestedJobName.ComplianceScans,
        unit: { value: 'd', disabled: false },
        threshold: [{ value: '30', disabled: false }, Validators.required],
        disabled: false
      }
    };
  }

}
