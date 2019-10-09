import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

import { ChefComponentsModule } from 'app/components/chef-components.module';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';

import { TeamManagementComponent } from './team-management/team-management.component';
import { TeamDetailsComponent } from './team-details/team-details.component';
import { TeamAddUsersComponent } from './team-add-users/team-add-users.component';

import { TeamRoutingModule } from './team-routing.module';

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    ChefComponentsModule,
    ChefPipesModule,
    TeamRoutingModule
  ],
  exports: [
    TeamManagementComponent,
    TeamDetailsComponent,
    TeamAddUsersComponent
  ],
  declarations: [
    TeamManagementComponent,
    TeamDetailsComponent,
    TeamAddUsersComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class ChefTeamModule { }
