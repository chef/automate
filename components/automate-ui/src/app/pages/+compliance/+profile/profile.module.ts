import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { ChefComponentsModule } from 'app/components/chef-components.module';
import { ComplianceSharedModule } from 'app/pages/+compliance/shared/shared.module';
import { ProfileDetailsComponent } from './+profile-details/profile-details.component';
import { ProfileOverviewComponent } from './+profile-overview/profile-overview.component';
import { ProfileRoutingModule } from './profile.routing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    ComplianceSharedModule,
    ChefComponentsModule,
    ProfileRoutingModule
  ],
  declarations: [
    ProfileDetailsComponent,
    ProfileOverviewComponent
  ],
  exports: [
    ProfileDetailsComponent,
    ProfileOverviewComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class ProfileModule {}
