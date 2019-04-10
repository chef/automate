import { Component } from '@angular/core';
import { Store } from '@ngrx/store';
import { Subscription } from 'rxjs';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LocalStorageService } from 'app/services/storage/localstorage.service';
import { SessionStorageService } from 'app/services/storage/sessionstorage.service';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { pendingState } from 'app/entities/entities';
import { TriggerWelcomeStatus } from 'app/entities/license/license.reducer';
import { triggerWelcome } from 'app/entities/license/license.selectors';

// Local storage keys
//// Stores the value set by WelcomeModalComponent.showAtStartPref
const SHOW_AT_START_PREF_KEY = 'show-welcome-modal-on-startup';

@Component({
  selector: 'app-welcome-modal',
  templateUrl: './welcome-modal.component.html',
  styleUrls: ['./welcome-modal.component.scss']
})
export class WelcomeModalComponent {

  // User preference to show the welcome modal on startup.
  // Set by the user via the 'Show this window on launch' checkbox in
  // the welcome modal.
  public showAtStartPref = false;

  // Is the Welcome modal currently visible.
  public isVisible = false;

  private statusSubscription: Subscription;

  private isTelemetryServiceEnabled: boolean;
  private telemetryServiceSubscription: Subscription;

  constructor(
    store: Store<NgrxStateAtom>,
    private localStorage: LocalStorageService,
    private sessionStorage: SessionStorageService,
    private chefSessionService: ChefSessionService,
    private telemetryService: TelemetryService
  ) {
    this.statusSubscription =
      store.select(triggerWelcome).subscribe(this.handleTriggerWelcome.bind(this));

    if (this.telemetryService.hasTelemetryResponse) {
      this.isTelemetryServiceEnabled = this.telemetryService.telemetryEnabled;
    } else {
      this.telemetryServiceSubscription =
        this.telemetryService.enabled.subscribe(telemetryEnabled => {
          this.isTelemetryServiceEnabled = telemetryEnabled;
        });
    }
}

  public handleTriggerWelcome(state: TriggerWelcomeStatus): void {

    // no code should be before the pending check!

    if (pendingState(state)) { return; }
    if (this.statusSubscription) {
      this.statusSubscription.unsubscribe(); // only execute once!
      // Getting here means we have been notified that
      // the license modal was NOT shown to the user,
      // so we should proceed with (maybe) showing this modal.
    }

    // If telemetry is enabled but preference not set yet, show the modal
    // again so users can set their individual telemetry preference.
    if (this.isTelemetryServiceEnabled &&
      this.chefSessionService.fetchTelemetryPreference() === null) {
      this.showModal();
      return;
    }

    // On init check if user has selected a preference for modal visibility on startup.
    switch (this.localStorage.getBoolean(SHOW_AT_START_PREF_KEY)) {
      case true:
        this.showAtStartPref = true;
        // app can be initialized multiple times per session; this shows only once per session
        this.maybeShowModal(this.showAtStartPref);
        break;
      case false:
        this.showAtStartPref = false;
        // We use maybeShowModal here again because there are instances where
        // we are required to show the modal regardless of user preference.
        this.maybeShowModal(this.showAtStartPref);
        break;
      case null:
        // If the user has not set a preference this is their first visit
        // with this browser. So we show them the modal and store the default pref.
        this.localStorage.putBoolean(SHOW_AT_START_PREF_KEY, this.showAtStartPref);
        this.showModal();
        break;
    }
  }

  // Toggles the user's preference to see the modal and stores
  // it in localStorage.
  public toggleShowAgain(): void {
    this.showAtStartPref =
      this.localStorage.putBoolean(SHOW_AT_START_PREF_KEY,
        !this.showAtStartPref);
  }

  // Closes the modal.
  public closeModal(): void {
    this.isVisible = false;

    if (this.telemetryServiceSubscription) {
      this.telemetryServiceSubscription.unsubscribe();
    }
  }

  // Shows the modal and sets the has_been_seen flag to true.
  public showModal(): void {
    this.isVisible = true;
    this.sessionStorage.putBoolean(this.chefSessionService.userWelcomeModalSeenKey(), true);
  }

  // Determines whether or not to show the modal based on the user's
  // preference, which is passed as an argument, AND whether or not
  // the modal has already been seen this session.
  private maybeShowModal(showPref: boolean): void {
    // The result of this query to sessionStorage should always result in
    // either a 'true' or a 'null' value but we are explicitly checking for
    // true here for clarity.
    if (this.sessionStorage.getBoolean(this.chefSessionService.MODAL_HAS_BEEN_SEEN_KEY) === true) {
      // If the modal has already been seen we return early and do nothing.
      return;
    } else if (showPref) {
      // If the modal has not been seen and the user has set a preference
      // to see the modal on startup then we show the modal.
      this.showModal();
      return;
    }

    // If the previous checks fail we do nothing.
  }

}
