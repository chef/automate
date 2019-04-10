import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

import { AppRoutingModule } from 'app/app-routing.module';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { ChefComponentsModule } from 'app/components/chef-components.module';
import { PolicyListComponent } from './list/policy-list.component';
import { PolicyDetailsComponent } from './details/policy-details.component';
import { PolicyAddMembersComponent } from './add-members/policy-add-members.component';

@NgModule({
  declarations: [
    PolicyListComponent,
    PolicyDetailsComponent,
    PolicyAddMembersComponent
  ],
  imports: [
    AppRoutingModule,
    ChefComponentsModule,
    ChefPipesModule,
    CommonModule,
    FormsModule,
    ReactiveFormsModule
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class PolicyModule {}
