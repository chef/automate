import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { ChefComponentsModule } from '../../../components/chef-components.module';
import { ComplianceSharedModule } from '../shared/shared.module';
import { ProfileDetailsComponent } from './+profile-details/profile-details.component';
import { ProfileSidebarComponent } from './+profile-sidebar/profile-sidebar.component';
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
    ProfileSidebarComponent,
    ProfileOverviewComponent
  ],
  exports: [
    ProfileDetailsComponent,
    ProfileSidebarComponent,
    ProfileOverviewComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class ProfileModule {}
