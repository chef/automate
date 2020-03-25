import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

import { ChefComponentsModule } from 'app/components/chef-components.module';

import { DailyCheckInComponent } from './daily-check-in/daily-check-in.component';
import { DashboardComponent } from './dashboard/dashboard.component';

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
    DashboardComponent
  ],
  declarations: [
    DailyCheckInComponent,
    DashboardComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class DesktopModule { }
