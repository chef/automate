import { Component, isDevMode, OnInit } from '@angular/core';
import { FeatureFlagsService } from '../../../app/services/feature-flags/feature-flags.service';

@Component({
  selector: 'app-navbar',
  templateUrl: './navbar.component.html',
  styleUrls: ['./navbar.component.scss']
})

export class NavbarComponent implements OnInit {
  public applicationsFeatureFlagOn: boolean;

  constructor(
    private featureFlagsService: FeatureFlagsService
  ) {}

  isDevMode() {
    return isDevMode();
  }

  ngOnInit() {
    this.applicationsFeatureFlagOn = this.featureFlagsService.getFeatureStatus('applications');
  }
}
