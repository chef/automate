import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

import { ChefComponentsModule } from 'app/components/chef-components.module';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';

import { NodesListComponent } from './nodes-list/nodes-list.component';

import { NodesRoutingModule } from './nodes-routing.module';

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    ChefComponentsModule,
    NodesRoutingModule,
    ChefPipesModule
  ],
  exports: [
    NodesListComponent
  ],
  declarations: [
    NodesListComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class NodesModule { }
