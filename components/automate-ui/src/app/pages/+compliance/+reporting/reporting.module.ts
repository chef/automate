import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { ChefComponentsModule } from '../../../components/chef-components.module';
import { ComplianceSharedModule } from '../shared/shared.module';
import { ReportingComponent } from './reporting.component';
import { ReportingSearchbarComponent } from './reporting-searchbar/reporting-searchbar.component';
import { ReportingRoutingModule } from './reporting.routing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    ComplianceSharedModule,
    ChefComponentsModule,
    ReportingRoutingModule
  ],
  declarations: [
    ReportingComponent,
    ReportingSearchbarComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ],
  exports: [
    ReportingComponent
  ]
})
export class ReportingModule {}
