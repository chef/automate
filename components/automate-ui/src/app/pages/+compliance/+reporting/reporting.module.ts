import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { ChefComponentsModule } from '../../../components/chef-components.module';
import { ComplianceSharedModule } from '../shared/shared.module';
import { ReportingComponent } from './reporting.component';
import { ReportingSearchbarComponent } from './reporting-searchbar/reporting-searchbar.component';
import { ReportingRoutingModule } from './reporting.routing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ChefPipesModule } from '../../../pipes/chef-pipes.module';
import { InfiniteScrollDirective } from 'ngx-infinite-scroll';

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    ComplianceSharedModule,
    ChefComponentsModule,
    ChefPipesModule,
    ReportingRoutingModule,
    InfiniteScrollDirective
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
