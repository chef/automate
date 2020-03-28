import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

import { ChefComponentsModule } from 'app/components/chef-components.module';

import { DailyCheckInComponent } from './daily-check-in/daily-check-in.component';
import { CheckInTimeSeriesComponent } from './check-in-time-series/check-in-time-series.component';
import { DashboardComponent } from './dashboard/dashboard.component';
import { TopErrorsComponent } from './top-errors/top-errors.component';

import { DesktopRoutingModule } from './desktop-routing.module';

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    ChefComponentsModule,
    DesktopRoutingModule
  ],
  exports: [
    DailyCheckInComponent,
    DashboardComponent,
    CheckInTimeSeriesComponent,
    TopErrorsComponent
  ],
  declarations: [
    DailyCheckInComponent,
    DashboardComponent,
    CheckInTimeSeriesComponent,
    TopErrorsComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class DesktopModule { }
