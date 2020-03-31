import { Component } from '@angular/core';

import { FeatureFlagsService } from '../../services/feature-flags/feature-flags.service';

@Component({
  selector: 'app-settings-sidebar',
  templateUrl: './settings-sidebar.component.html',
  styleUrls: ['./settings-sidebar.component.scss']
})

export class SettingsSidebarComponent {
  featureFlagOn: boolean;

  constructor(
    private featureFlags: FeatureFlagsService
    ) {
    this.featureFlagOn = this.featureFlags.getFeatureStatus('servicenow_cmdb');
  }
}
