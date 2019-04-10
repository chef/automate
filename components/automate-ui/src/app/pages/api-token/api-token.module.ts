import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

import { AppRoutingModule } from 'app/app-routing.module';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { ChefComponentsModule } from 'app/components/chef-components.module';

import { ApiTokenListComponent } from './list/api-token-list.component';
import { ApiTokenDetailsComponent } from './details/api-token-details.component';


@NgModule({
  declarations: [
    ApiTokenDetailsComponent,
    ApiTokenListComponent
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
export class ApiTokenModule {}
