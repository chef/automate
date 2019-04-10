import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { ReportingProfileComponent } from './reporting-profile.component';
import { ReportingProfileRoutingModule } from './reporting-profile.routing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ComplianceSharedModule } from '../../shared/shared.module';
import { ChefComponentsModule } from '../../../../components/chef-components.module';

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    ReportingProfileRoutingModule,
    ComplianceSharedModule,
    ChefComponentsModule
  ],
  declarations: [
    ReportingProfileComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ],
  exports: [
    ReportingProfileComponent
  ]
})
export class ReportingProfileModule {}
