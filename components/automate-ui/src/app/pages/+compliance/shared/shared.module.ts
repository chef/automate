import { NgModule, CUSTOM_ELEMENTS_SCHEMA , ModuleWithProviders } from '@angular/core';
import { CommonModule } from '@angular/common';

import { ChefComponentsModule } from 'app/components/chef-components.module';
import { StatsService } from './reporting';
import { ReportQueryService } from './reporting/report-query.service';
import { ReportDataService } from './reporting/report-data.service';
import { ScanResultsService } from './reporting/scan-results.service';

@NgModule({
  imports: [
    CommonModule,
    ChefComponentsModule
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ],
  exports: [
    ChefComponentsModule
  ]
})
export class ComplianceSharedModule {
  static forRoot(): ModuleWithProviders<ComplianceSharedModule> {
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
