import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

import { ChefComponentsModule } from 'app/components/chef-components.module';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';

import { DailyCheckInComponent } from './daily-check-in/daily-check-in.component';
import { CheckInTimeSeriesComponent } from './check-in-time-series/check-in-time-series.component';
import { DashboardComponent } from './dashboard/dashboard.component';
import { DesktopDetailComponent } from './desktop-detail/desktop-detail.component';
import { SimpleLineGraphComponent } from 'app/page-components/simple-line-graph/simple-line-graph.component';
import { TopErrorsComponent } from './top-errors/top-errors.component';
import {
  UnknownDesktopDurationCountsComponent
} from './unknown-desktop-duration-counts/unknown-desktop-duration-counts.component';
import { InsightComponent } from './insight/insight.component';
import {
  InsightAttributesDropdownComponent
} from 'app/page-components/insight-attributes-dropdown/insight-attributes-dropdown.component';
import { EmptyDataComponent } from './empty-data/empty-data.component';
import { DesktopRoutingModule } from './desktop-routing.module';

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    ChefComponentsModule,
    DesktopRoutingModule,
    ChefPipesModule
  ],
  exports: [
    CheckInTimeSeriesComponent,
    DailyCheckInComponent,
    DashboardComponent,
    EmptyDataComponent,
    InsightComponent,
    InsightAttributesDropdownComponent,
    SimpleLineGraphComponent,
    TopErrorsComponent,
    UnknownDesktopDurationCountsComponent
  ],
  declarations: [
    CheckInTimeSeriesComponent,
    DailyCheckInComponent,
    DashboardComponent,
    DesktopDetailComponent,
    EmptyDataComponent,
    InsightComponent,
    InsightAttributesDropdownComponent,
    SimpleLineGraphComponent,
    TopErrorsComponent,
    UnknownDesktopDurationCountsComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class DesktopModule { }
