import { Component, OnInit } from '@angular/core';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { ServiceGroupsFacadeService } from 'app/entities/service-groups/service-groups.facade';

@Component({
  selector: 'app-applications-dashboard',
  templateUrl: './applications.component.html',
  styleUrls: ['./applications.component.scss']
})

export class ApplicationsComponent implements OnInit {
  public applicationsFeatureFlagOn: boolean;

  constructor(
    private featureFlagsService: FeatureFlagsService,
    private facade: ServiceGroupsFacadeService
  ) {}

  ngOnInit() {
    this.facade.showSidebar();
    this.applicationsFeatureFlagOn = this.featureFlagsService.getFeatureStatus('applications');
  }
}
