import {
  Component,
  HostListener,
  Input,
  OnInit
} from '@angular/core';
import { sortBy } from 'lodash/fp';

import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { Feature } from 'app/services/feature-flags/types';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

const codeLength = 4;

export enum FlagTypes {
  beta = 'beta',
  beta_or_experimental = 'beta or experimental',
  legacy = 'legacy'
}

@Component({
  selector: 'app-feature-flags',
  templateUrl: './feature-flags.component.html',
  styleUrls: ['./feature-flags.component.scss'],
  providers: [FeatureFlagsService]
})
export class FeatureFlagsComponent implements OnInit {

  keysCache: Array<number> = [];
  features: Array<Feature> = [];
  // Note: all codes must be codeLength characters long (see above) and UPPERCASE
  @Input() experimentalCode = 'FEAT';
  @Input() experimentalFeatures: Array<Feature> = [];
  @Input() betaCode = 'BETA';
  @Input() betaFeatures: Array<Feature> = [];
  @Input() legacyCode = 'LEGA';
  @Input() legacyFeatures: Array<Feature> = [];
  isVisible = false;

  public warning: string;
  public flagType: FlagTypes;

  betaWarning = 'The flagged features below have not yet been made generally available. If you ' +
                'choose to turn them on, be aware that they may contain bugs that could reduce ' +
                'performance, make the experience poor, or break everything. The warranties in ' +
                'your license agreement do not apply to the flagged features – they are ' +
                'provided "as is" with no warranties of any kind, whether express or implied, ' +
                'and Chef will have no liability to you for any problems in or caused by the ' +
                'flagged features or any related documentation, whether direct, indirect, ' +
                'special or consequential, including lost profits or data (our lawyer made us ' +
                'add the last part).';

  legacyWarning = 'The flagged features below are in legacy status, meaning they have been ' +
                  'replaced with new functionality in the product.  Legacy features will be ' +
                  'available in releases for 90 days from the date of the release in which ' +
                  'they were replaced.  If you find yourself relying on a legacy feature, ' +
                  'please contact us at http://chef.io/feedback so we can understand why the ' +
                  'replacement feature does not meet your needs.';

  constructor(
    public featureFlagsService: FeatureFlagsService,
    private telemetryService: TelemetryService
  ) {}

  ngOnInit() {
    this.betaFeatures
      .concat(this.experimentalFeatures)
      .concat(this.legacyFeatures)
      .forEach(feature =>
        this.featureFlagsService.setFeature(feature.key,
          this.featureFlagsService.getFeatureStatus(feature.key))
        );
  }

  updateFlag(feature: Feature): void {
    const newStatus = !this.featureFlagsService.getFeatureStatus(feature.key);
    this.telemetryService.track('featureFlags', { feature: feature, status: newStatus});
    this.featureFlagsService.setFeature(feature.key, newStatus);
  }

  @HostListener('document: keyup', ['$event.keyCode'])
  handleKeyUp(keyCode: number): void {
    // when reaching codeLength, drop first item
    if (this.keysCache.push(keyCode) === codeLength + 1) {
      this.keysCache.shift();
    }

    switch (String.fromCharCode(...this.keysCache)) {
      case this.betaCode:
        this.features = sortBy('name', this.betaFeatures);
        this.warning = this.betaWarning;
        this.isVisible = !this.isVisible || this.flagType !== FlagTypes.beta;
        this.flagType = FlagTypes.beta;
        break;
      case this.experimentalCode:
        this.features = sortBy('name', this.betaFeatures.concat(this.experimentalFeatures));
        this.warning = this.betaWarning;
        this.isVisible = !this.isVisible || this.flagType !== FlagTypes.beta_or_experimental;
        this.flagType = FlagTypes.beta_or_experimental;
        break;
      case this.legacyCode:
        this.features = sortBy('name', this.legacyFeatures);
        this.warning = this.legacyWarning;
        this.isVisible = !this.isVisible || this.flagType !== FlagTypes.legacy;
        this.flagType = FlagTypes.legacy;
        break;
    }
  }

  closeModal(): void {
    this.isVisible = false;
  }
}
