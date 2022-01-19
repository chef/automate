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
import { CreateClientModalComponent } from './create-client-modal/create-client-modal.component';
import { CookbooksComponent } from './cookbooks/cookbooks.component';
import { CookbookDetailsComponent } from './cookbook-details/cookbook-details.component';
import { CookbookDependenciesComponent } from './cookbook-dependencies/cookbook-dependencies.component';
import { CookbookDependenciesDetailsComponent } from './cookbook-dependencies-details/cookbook-dependencies-details.component';
import { CreateChefServerSliderComponent } from './create-chef-server-slider/create-chef-server-slider.component';
import { CreateEnvironmentModalComponent } from './create-environment-modal/create-environment-modal.component';
import { CreateOrgModalComponent } from './create-org-modal/create-org-modal.component';
import { CreateDataBagModalComponent } from './create-data-bag-modal/create-data-bag-modal.component';
import { CreateDatabagItemModalComponent } from './create-databag-item-modal/create-databag-item-modal.component';
import { CreateInfraRoleModalComponent } from './create-infra-role-modal/create-infra-role-modal.component';
import { DataBagsDetailsComponent } from './data-bags-details/data-bags-details.component';
import { DataBagsListComponent } from './data-bags-list/data-bags-list.component';
import { DeleteInfraObjectModalComponent } from './delete-infra-object-modal/delete-infra-object-modal.component';
import { EditDataBagItemModalComponent } from './edit-data-bag-item-modal/edit-data-bag-item-modal.component';
import { EditEnvironmentAttributeModalComponent } from './edit-environment-attribute-modal/edit-environment-attribute-modal.component';
import { EditInfraNodeModalComponent } from './edit-infra-node-modal/edit-infra-node-modal.component';
import { EditInfraRoleModalComponent } from './edit-infra-role-modal/edit-infra-role-modal.component';
import { EditInfraNodeAttributeModalComponent } from './edit-infra-node-attribute-modal/edit-infra-node-attribute-modal.component';
import { EmptyStateComponent } from './empty-state/empty-state.component';
import { EnvironmentsComponent } from './environments/environments.component';
import { EnvironmentDetailsComponent } from './environment-details/environment-details.component';
import { IncludedPoliciesDetailsComponent } from './included-policies-details/included-policies-details.component';
import { InfraEnvironmentConstraintComponent } from './infra-environment-constraint/infra-environment-constraint.component';
import { InfraNodesComponent } from './infra-nodes/infra-nodes.component';
import { InfraNodeDetailsComponent } from './infra-node-details/infra-node-details.component';
import { InfraRolesComponent } from './infra-roles/infra-roles.component';
import { InfraRoleDetailsComponent } from './infra-role-details/infra-role-details.component';
import { InfraSearchBarComponent } from './infra-search-bar/infra-search-bar.component';
import { InfraTabComponent } from './infra-tab-change/infra-tab/infra-tab.component';
import { InfraTabChangeComponent } from './infra-tab-change/infra-tab-change.component';
import { JsonTreeTableComponent } from './json-tree-table/json-tree-table.component';
import { OrgDetailsComponent } from './org-details/org-details.component';
import { OrgEditComponent } from './org-edit/org-edit.component';
import { PaginationComponent } from './pagination/components/pagination.component';
import { PolicyFilesComponent } from './policy-files/policy-files.component';
import { PolicyFileDetailsComponent } from './policy-file-details/policy-file-details.component';
import { PolicyGroupsComponent } from './policy-groups/policy-groups.component';
import { PolicyGroupsListComponent } from './policy-groups-list/policy-groups-list.component';
import { PolicyGroupDetailsComponent } from './policy-group-details/policy-group-details.component';
import { ResetAdminKeyComponent } from './reset-admin-key/reset-admin-key.component';
import { ResetClientKeyComponent } from './reset-client-key/reset-client-key.component';
import { ResetNodeKeyComponent } from './reset-node-key/reset-node-key.component';
import { RevisionIdComponent } from './revision-id/revision-id.component';
import { UpdateNodeTagModalComponent } from './update-node-tag-modal/update-node-tag-modal.component';
import { DragDropModule } from '@angular/cdk/drag-drop';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatTabsModule } from '@angular/material/tabs';
import { NgSelectModule } from '@ng-select/ng-select';
import { PaginatorComponent } from './paginator/paginator.component';
import { SelectBoxModule } from './select-box/src/public_api';
import { SyncOrgUsersSliderComponent } from './sync-org-users-slider/sync-org-users-slider.component';
import { TreeTableModule } from './tree-table/tree-table.module';
import { UpdateWebUIKeySliderComponent } from './update-web-uikey-slider/update-web-uikey-slider.component';

@NgModule({
  declarations: [
    ChefServersListComponent,
    ChefServerDetailsComponent,
    ClientsComponent,
    ClientDetailsComponent,
    CookbooksComponent,
    CookbookDetailsComponent,
    CookbookDependenciesComponent,
    CookbookDependenciesDetailsComponent,
    CreateChefServerSliderComponent,
    CreateEnvironmentModalComponent,
    CreateOrgModalComponent,
    CreateDataBagModalComponent,
    CreateClientModalComponent,
    CreateDatabagItemModalComponent,
    CreateInfraRoleModalComponent,
    DataBagsDetailsComponent,
    DataBagsListComponent,
    DeleteInfraObjectModalComponent,
    EditDataBagItemModalComponent,
    EditEnvironmentAttributeModalComponent,
    EditInfraNodeModalComponent,
    EditInfraRoleModalComponent,
    EditInfraNodeAttributeModalComponent,
    EmptyStateComponent,
    EnvironmentsComponent,
    EnvironmentDetailsComponent,
    JsonTreeTableComponent,
    IncludedPoliciesDetailsComponent,
    InfraEnvironmentConstraintComponent,
    InfraNodesComponent,
    InfraNodeDetailsComponent,
    InfraRolesComponent,
    InfraRoleDetailsComponent,
    InfraSearchBarComponent,
    InfraTabComponent,
    InfraTabChangeComponent,
    OrgDetailsComponent,
    OrgEditComponent,
    PaginationComponent,
    PaginatorComponent,
    PolicyFilesComponent,
    PolicyFileDetailsComponent,
    PolicyGroupsComponent,
    PolicyGroupsListComponent,
    PolicyGroupDetailsComponent,
    ResetAdminKeyComponent,
    ResetClientKeyComponent,
    ResetNodeKeyComponent,
    RevisionIdComponent,
    SyncOrgUsersSliderComponent,
    UpdateNodeTagModalComponent,
    UpdateWebUIKeySliderComponent
  ],
  imports: [
    CommonModule,
    AppRoutingModule,
    ChefComponentsModule,
    ChefPipesModule,
    DragDropModule,
    SelectBoxModule,
    TreeTableModule,
    FormsModule,
    MatTabsModule,
    MatInputModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatPaginatorModule,
    NgSelectModule
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class InfraProxyModule { }
