import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

import { AppRoutingModule } from 'app/app-routing.module';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { ChefComponentsModule } from 'app/components/chef-components.module';
import { ChefServerDetailsComponent } from './chef-server-details/chef-server-details.component';
import { ChefServersListComponent } from './chef-servers-list/chef-servers-list.component';
import { ClientsComponent } from './clients/clients.component';
import { ClientDetailsComponent } from './client-details/client-details.component';
import { CookbooksComponent } from './cookbooks/cookbooks.component';
import { CookbookDetailsComponent } from './cookbook-details/cookbook-details.component';
import { CreateChefServerModalComponent } from './create-chef-server-modal/create-chef-server-modal.component';
import { CreateOrgModalComponent } from './create-org-modal/create-org-modal.component';
import { DataBagsDetailsComponent } from './data-bags-details/data-bags-details.component';
import { DataBagsListComponent } from './data-bags-list/data-bags-list.component';
import { EnvironmentsComponent } from './environments/environments.component';
import { EnvironmentDetailsComponent } from './environment-details/environment-details.component';
import { InfraNodesComponent } from './infra-nodes/infra-nodes.component';
import { InfraRolesComponent } from './infra-roles/infra-roles.component';
import { InfraRoleDetailsComponent } from './infra-role-details/infra-role-details.component';
import { JsonTreeTableComponent } from './json-tree-table/json-tree-table.component';
import { OrgDetailsComponent } from './org-details/org-details.component';
import { OrgEditComponent } from './org-edit/org-edit.component';
import { PolicyFilesComponent } from './policy-files/policy-files.component';
import { ResetAdminKeyComponent } from './reset-admin-key/reset-admin-key.component';
import { TreeTableModule } from './tree-table/tree-table.module';
import { EmptyStateComponent } from './empty-state/empty-state.component';

@NgModule({
  declarations: [
    ChefServersListComponent,
    ChefServerDetailsComponent,
    ClientsComponent,
    ClientDetailsComponent,
    CookbooksComponent,
    CookbookDetailsComponent,
    CreateChefServerModalComponent,
    CreateOrgModalComponent,
    DataBagsDetailsComponent,
    DataBagsListComponent,
    EmptyStateComponent,
    EnvironmentsComponent,
    EnvironmentDetailsComponent,
    JsonTreeTableComponent,
    InfraNodesComponent,
    InfraRolesComponent,
    InfraRoleDetailsComponent,
    OrgDetailsComponent,
    OrgEditComponent,
    PolicyFilesComponent,
    ResetAdminKeyComponent
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
