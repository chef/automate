import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { ComplianceSharedModule } from '../../shared/shared.module';
import { ReportingProfilesComponent } from './reporting-profiles.component';
import { ReportingProfilesRoutingModule } from './reporting-profiles.routing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    ComplianceSharedModule,
    ReportingProfilesRoutingModule
  ],
  declarations: [
    ReportingProfilesComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ],
  exports: [
    ReportingProfilesComponent
  ]
})
export class ReportingProfilesModule {}
