import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { OverviewFailuresComponent } from './overview-failures/overview-failures.component';
import { OverviewSeverityComponent } from './overview-severity/overview-severity.component';
import { OverviewStatusComponent } from './overview-status/overview-status.component';
import { OverviewTrendComponent } from './overview-trend/overview-trend.component';
import { ReportingOverviewComponent } from './reporting-overview.component';
import { ReportingOverviewRoutingModule } from './reporting-overview.routing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    ReportingOverviewRoutingModule
  ],
  declarations: [
    ReportingOverviewComponent,
    OverviewFailuresComponent,
    OverviewSeverityComponent,
    OverviewStatusComponent,
    OverviewTrendComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ],
  exports: [
    ReportingOverviewComponent
  ]
})
export class ReportingOverviewModule {}
