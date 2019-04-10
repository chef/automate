import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { ReportingNodesComponent } from './reporting-nodes.component';
import { ReportingNodesRoutingModule } from './reporting-nodes.routing';
import { ComplianceSharedModule } from '../../shared/shared.module';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    ReportingNodesRoutingModule,
    ComplianceSharedModule
  ],
  declarations: [
    ReportingNodesComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ],
  exports: [
    ReportingNodesComponent
  ]
})
export class ReportingNodesModule {}
