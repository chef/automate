import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

import { AppRoutingModule } from 'app/app-routing.module';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { ChefComponentsModule } from 'app/components/chef-components.module';
import { ChefServersListComponent } from './chef-servers-list/chef-servers-list.component';
import { CreateChefServerModalComponent } from './create-chef-server-modal/create-chef-server-modal.component';
import { ChefServerDetailsComponent } from './chef-server-details/chef-server-details.component';
import { CreateOrgModalComponent } from './create-org-modal/create-org-modal.component';

@NgModule({
  declarations: [
    ChefServersListComponent,
    CreateChefServerModalComponent,
    ChefServerDetailsComponent,
    CreateOrgModalComponent
  ],
  imports: [
    CommonModule,
    AppRoutingModule,
    ChefComponentsModule,
    ChefPipesModule,
    FormsModule,
    ReactiveFormsModule
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class InfraProxyModule { }
