import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

import { AppRoutingModule } from '../../app-routing.module';
import { ChefPipesModule } from '../../pipes/chef-pipes.module';
import { ChefComponentsModule } from '../../components/chef-components.module';
import { RolesListComponent } from './list/roles-list.component';
import { RoleDetailsComponent } from './details/role-details.component';

@NgModule({
  declarations: [
    RolesListComponent,
    RoleDetailsComponent
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
export class RoleModule {}
