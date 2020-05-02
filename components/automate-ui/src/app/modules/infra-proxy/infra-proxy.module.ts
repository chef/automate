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
import { CookbookDetailsComponent } from './cookbook-details/cookbook-details.component';
import { CreateChefServerModalComponent } from './create-chef-server-modal/create-chef-server-modal.component';
import { CreateOrgModalComponent } from './create-org-modal/create-org-modal.component';
import { TreeTableModule } from './tree-table/tree-table.module';
import { JsonTreeTableComponent } from './json-tree-table/json-tree-table.component';

@NgModule({
  declarations: [
    ChefServersListComponent,
    ChefServerDetailsComponent,
    OrgDetailsComponent,
    InfraRoleDetailsComponent,
    CookbookDetailsComponent,
    CreateChefServerModalComponent,
    CreateOrgModalComponent,
    JsonTreeTableComponent
  ],
  imports: [
    CommonModule,
    AppRoutingModule,
    ChefComponentsModule,
    ChefPipesModule,
    TreeTableModule,
    FormsModule,
    ReactiveFormsModule
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class InfraProxyModule { }
