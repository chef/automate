import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

import { AppRoutingModule } from 'app/app-routing.module';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { ChefComponentsModule } from 'app/components/chef-components.module';
import { ChefServerDetailsComponent } from './chef-server-details/chef-server-details.component';
import { ChefServersListComponent } from './chef-servers-list/chef-servers-list.component';
import { OrgDetailsComponent } from './org-details/org-details.component';
import { InfraRoleDetailsComponent } from './infra-role-details/infra-role-details.component';
import { CreateChefServerModalComponent } from './create-chef-server-modal/create-chef-server-modal.component';
import { CreateOrgModalComponent } from './create-org-modal/create-org-modal.component';
import { TreetableModule } from './treetable/treetable.module';

@NgModule({
  declarations: [
    ChefServersListComponent,
    ChefServerDetailsComponent,
    OrgDetailsComponent,
    InfraRoleDetailsComponent,
    CreateChefServerModalComponent,
    CreateOrgModalComponent
  ],
  imports: [
    CommonModule,
    AppRoutingModule,
    ChefComponentsModule,
    ChefPipesModule,
    TreetableModule,
    FormsModule,
    ReactiveFormsModule
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class InfraProxyModule { }
