import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { ChefComponentsModule } from 'app/components/chef-components.module';
import { ComplianceComponent } from './compliance.component';
import { ComplianceRoutingModule } from './compliance.routing';
import {
  ComplianceLandingComponent
} from 'app/pages/compliance-landing/compliance-landing.component';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    ComplianceRoutingModule,
    ChefComponentsModule
  ],
  declarations: [
    ComplianceComponent,
    ComplianceLandingComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class ComplianceModule {}
