import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

import { AppRoutingModule } from 'app/app-routing.module';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { ChefComponentsModule } from 'app/components/chef-components.module';
import { ChefServersListComponent } from './chef-servers-list/chef-servers-list.component';
import { ChefServersDetailsComponent } from './chef-servers-details/chef-servers-details.component';
import { OrgCreateModalComponent } from './org-create-modal/org-create-modal.component';
import { CreateChefServerModalComponent } from './create-chef-server-modal/create-chef-server-modal.component';
import { CookbooksListComponent } from './cookbook-list/cookbooks-list.component';

@NgModule({
  declarations: [
    ChefServersListComponent,
    ChefServersDetailsComponent,
    OrgCreateModalComponent,
    CreateChefServerModalComponent,
    CookbooksListComponent
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
