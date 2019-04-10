import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { ComplianceComponent } from './compliance.component';
import { ComplianceRoutingModule } from './compliance.routing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    ComplianceRoutingModule
  ],
  declarations: [
    ComplianceComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class ComplianceModule {}
