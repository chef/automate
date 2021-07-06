import { Component, OnInit, OnDestroy } from '@angular/core';
import { isNil } from 'lodash';
import { Subscription } from 'rxjs';

import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-telemetry-checkbox',
  templateUrl: './telemetry-checkbox.component.html',
  styleUrls: ['./telemetry-checkbox.component.scss']
})
export class TelemetryCheckboxComponent implements OnInit, OnDestroy {

  // The user's preference to enable telemetry.
  // Bound to the telemetry checkbox in the welcome modal.
  telemetryPersonalPref = true;

  // Whether or not to show the telemetry preference.
  // Should only show when telemetry has been enabled on the server.
  public isTelemetryServiceEnabled: boolean;

  telemetryServiceSubscription: Subscription;

  constructor(
    private telemetryService: TelemetryService,
    private chefSessionService: ChefSessionService
  ) { }

  ngOnInit() {
    this.initializeTelemetryResponse();
  }

  ngOnDestroy() {
    if (this.isTelemetryServiceEnabled) {
      this.telemetryService.setUserTelemetryPreference(this.telemetryPersonalPref);
    }

    // Ends the subscription if needed
    if (this.telemetryServiceSubscription) {
      this.telemetryServiceSubscription.unsubscribe();
    }
  }

  initializeTelemetryResponse() {
    // Check if we have gotten the response from the server for the telemetry preference
    if (this.telemetryService.hasTelemetryResponse) {
      this.setTelemetryPreferences(this.telemetryService.telemetryEnabled);
    } else {
      // If the telemetry service is enabled we need to show the telemetry
      // checkbox to the user and grab the user's preference from the server.
      this.telemetryServiceSubscription =
        this.telemetryService.enabled.subscribe(telemetryEnabled => {
          this.setTelemetryPreferences(telemetryEnabled);
        });
    }
  }

  setTelemetryPreferences(telemetryEnabled) {
    this.isTelemetryServiceEnabled = telemetryEnabled;
    const telemetryPref = this.chefSessionService.telemetry_enabled;
    if (!isNil(telemetryPref)) {
      this.telemetryPersonalPref = telemetryPref;
    }
  }

  togglePersonalTelemetryCollection(): void {
    this.telemetryPersonalPref = !this.telemetryPersonalPref;
    this.telemetryService.setUserTelemetryPreference(this.telemetryPersonalPref);
  }
}
