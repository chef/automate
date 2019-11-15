import { Component, OnInit } from '@angular/core';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';

@Component({
  selector: 'app-applications-dashboard',
  templateUrl: './applications.component.html',
  styleUrls: ['./applications.component.scss']
})

export class ApplicationsComponent implements OnInit {
  public applicationsFeatureFlagOn: boolean;

  constructor(
    private featureFlagsService: FeatureFlagsService,
    private layoutFacade: LayoutFacadeService
  ) {}

  ngOnInit() {
    this.layoutFacade.showApplicationsSidebar();
    this.applicationsFeatureFlagOn = this.featureFlagsService.getFeatureStatus('applications');
  }
}
