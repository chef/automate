import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';


import { DashboardComponent } from './dashboard/dashboard.component';
import { CdsRoutingModule } from './cds-routing.module';

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    CdsRoutingModule
  ],
  exports: [
    DashboardComponent
  ],
  declarations: [
    DashboardComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class CdsModule { }
