import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { ComplianceSharedModule } from '../../shared/shared.module';
import { ReportingControlsComponent } from './reporting-controls.component';
import { ReportingControlsRoutingModule } from './reporting-controls.routing';

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    ComplianceSharedModule,
    ChefPipesModule,
    ReportingControlsRoutingModule
  ],
  declarations: [
    ReportingControlsComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ],
  exports: [
    ReportingControlsComponent
  ]
})
export class ReportingControlsModule {}
