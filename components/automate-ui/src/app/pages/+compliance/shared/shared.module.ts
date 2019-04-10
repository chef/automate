import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ChefComponentsModule } from '../../../components/chef-components.module';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { StatsService } from './reporting';
import { ReportQueryService } from './reporting/report-query.service';
import { ReportDataService } from './reporting/report-data.service';
import { ScanResultsService } from './reporting/scan-results.service';
import {
  ComplianceReportingSidebarComponent
} from '../compliance-reporting-sidebar/compliance-reporting-sidebar.component';

@NgModule({
  imports: [
    CommonModule,
    ChefComponentsModule
  ],
  declarations: [
    ComplianceReportingSidebarComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ],
  exports: [
    ChefComponentsModule,
    ComplianceReportingSidebarComponent
  ]
})
export class ComplianceSharedModule {
  static forRoot() {
    return {
      ngModule: ComplianceSharedModule,
      providers: [
        StatsService,
        ReportQueryService,
        ReportDataService,
        ScanResultsService
      ]
    };
  }
}
