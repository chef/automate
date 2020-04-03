import { AfterViewInit, Component, EventEmitter, Output } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Subscription } from 'rxjs';
import * as moment from 'moment';

import { LicenseFacadeService, LicenseApplyReason } from 'app/entities/license/license.facade';

import { environment } from 'environments/environment';
import { HttpStatus } from 'app/types/types';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { EntityStatus, pendingState } from 'app/entities/entities';

import {
  FetchStatus, RequestStatus
} from 'app/entities/license/license.model';
import { LicenseStatus, parsedExpirationDate } from 'app/entities/license/license.model';
import { SessionStorageService } from 'app/services/storage/sessionstorage.service';

@Component({
  selector: 'app-license-lockout',
  templateUrl: './license-lockout.component.html',
  styleUrls: ['./license-lockout.component.scss']
})
export class LicenseLockoutComponent implements AfterViewInit {
  @Output() triggerApplyLicense = new EventEmitter<LicenseApplyReason>();

  // Should never be on in production. Modify environment.ts locally
  // if you wish to bypass getting a session from dex or a license check
  private DISABLE_LICENSE_CHECK = environment.use_default_session;

  private subscriptions: Subscription[] = [];

  public trialForm: FormGroup;

  public gdprAgree = false; // for separate handling of gdpr checkbox
  public mlsaAgree = false;
  public requestingLicense = false;

  public licenseExpired = false;


  // error flags
  public fetchStatusInternalError = false;
  public trialRequestInternalError = false;
  public trialRequestConnectivityError = false;
  public permissionDenied = false;

  // for rendering the trial success part of the modal
  public trialLicenseApplied = false;

  // for displaying on confirmation
  public expirationDate: string;

  public modalVisible = false;
  public modalLocked = true;

  private firstTriggerCompleted = false;

  ngAfterViewInit(): void {
    if (!this.DISABLE_LICENSE_CHECK) {
      this.clearErrors();
      this.licenseFacade.getLicenseStatus();
    }
    // Set locale-aware date/time formats
    // Per https://stackoverflow.com/a/50460605, navigator.language
    // seems to be the best source of truth for browser locale.
    moment.locale(window.navigator.language);

    // ordinarily, chef-modal should autofocus on itself
    // this does not always work on the license-lockout component due to initial page load times
    // so this ensures the right element is focused on load
    const focusForm = setInterval(() => {

      const firstFormField = document.getElementById('firstName');
      if (firstFormField) {
        firstFormField.focus();
      }

      if (firstFormField === document.activeElement) {
        clearInterval(focusForm);
      }
    }, 1);
  }

  constructor(
    public licenseFacade: LicenseFacadeService,
    private chefSessionService: ChefSessionService,
    private sessionStorage: SessionStorageService,
    public fb: FormBuilder
  ) {
    this.trialForm = fb.group({
      firstName: ['', [Validators.required]],
      lastName: ['', [Validators.required]],
      email: ['', [Validators.required, Validators.email]]
    });
    // keep this subscription always present
    this.licenseFacade.fetchLicense$.subscribe(this.handleFetchLicenseStatus.bind(this));
  }

  private startListening() {
    this.subscriptions.push(
      this.licenseFacade.requestLicense$.subscribe(this.handleLicenseRequest.bind(this))
    );
  }

  private stopListening() {
    this.subscriptions.forEach((sub) => sub.unsubscribe());
  }

  public onTrialLicenseFormSubmit(): void {
    const formValues = this.trialForm.value;
    const trialLicenseRequest = {
      first_name: formValues.firstName.trim(),
      last_name: formValues.lastName.trim(),
      email: formValues.email.trim(),
      gdpr_agree: this.gdprAgree
    };

    this.clearErrors();
    this.requestingLicense = true; // round-trip initiating...
    this.licenseFacade.requestLicense(trialLicenseRequest);
  }

  public retryLicenseStatus() {
    this.closeModal();
    this.licenseFacade.getLicenseStatus();
  }

  public logout() {
    this.chefSessionService.logout();
  }

  // TODO: implement chef-checkbox as a ControlValueAccessor so it can be used by FormGroup
  // until then, must handle checkbox outside form logic
  public updateGDPR(event): void {
    this.gdprAgree = event;
  }

  // Similarly, we must manually add this to the valid logic
  // in the submit button since chef-button doesn't play nice
  // with FormBuilder.
  public updateMLSA(event): void {
    this.mlsaAgree = event;
  }

  public backToLicenseApply(reason?: LicenseApplyReason): void {
    this.closeModal();
    this.licenseFacade.updateLicenseApplyReason(reason);
    this.triggerApplyLicense.emit(reason);
  }

  private handleFetchLicenseStatus(state: FetchStatus): void {
    if (pendingState(state)) { return; }

    let licenseCurrentlyExpired = false;
    let noLicenseFound = false;

    if (state.status === EntityStatus.loadingSuccess) {
      this.licenseExpired = moment().isAfter(state.license.licensed_period.end);
      licenseCurrentlyExpired = this.licenseExpired;
      this.setExpirationDate(state.license);
    } else { // loadingFailure
      this.trialLicenseApplied = false;

      // No license returns HTTP status NOT_FOUND.
      // Any other HTTP status indicates some system issue (network, process, etc.).
      this.fetchStatusInternalError = (state.errorResp.status !== HttpStatus.NOT_FOUND);
      noLicenseFound = true;
    }
    if (noLicenseFound) {
      this.openModal();
    } else if (licenseCurrentlyExpired) {
      // don't open, trigger event to apply license
      this.backToLicenseApply(LicenseApplyReason.LICENSE_EXPIRED);
    } else if (!this.firstTriggerCompleted) {
      this.licenseFacade.triggerWelcome();
    }
    // After this function has been run once, never want to trigger
    // the welcome modal again, so keep track of that here.
    this.firstTriggerCompleted = true;
  }

  public handleLicenseRequest(state: RequestStatus): void {
    if (pendingState(state) || !this.requestingLicense) { return; }

    this.requestingLicense = false; // round-trip complete

    if (state.status === EntityStatus.loadingSuccess) {
      this.trialLicenseApplied = true;
      this.unlockModal();
      this.licenseFacade.getLicenseStatus();
      return;
    }

    // loadingFailure:
    switch (state.errorResp.status) {
      case HttpStatus.FORBIDDEN:
        this.permissionDenied = true;
        break;
      case HttpStatus.PRECONDITION_FAILED:
        this.trialRequestConnectivityError = true;
        break;
      default:
        this.trialRequestInternalError = true;
    }
  }

  public openModal() {
    this.startListening();
    this.modalVisible = true;
    this.sessionStorage.putBoolean(this.chefSessionService.userWelcomeModalSeenKey(), true);
  }

  public closeModal(): void {
    this.stopListening();
    this.modalVisible = false;
  }

  private unlockModal() {
    this.modalLocked = false;
  }

  private clearErrors(): void {
    this.fetchStatusInternalError = false;
    this.trialRequestInternalError = false;
    this.trialRequestConnectivityError = false;
    this.permissionDenied = false;
    this.licenseExpired = false;
  }

  private setExpirationDate(license: LicenseStatus): void {
    this.expirationDate = parsedExpirationDate(license);
  }
}
