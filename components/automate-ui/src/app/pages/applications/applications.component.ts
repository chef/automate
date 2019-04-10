import { Component, OnInit } from '@angular/core';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';

@Component({
  selector: 'app-applications-dashboard',
  templateUrl: './applications.component.html',
  styleUrls: ['./applications.component.scss']
})

export class ApplicationsComponent implements OnInit {
  public applicationsFeatureFlagOn: boolean;

  constructor(
    private featureFlagsService: FeatureFlagsService
  ) {}

  ngOnInit() {
    this.applicationsFeatureFlagOn = this.featureFlagsService.getFeatureStatus('applications');
  }
}
