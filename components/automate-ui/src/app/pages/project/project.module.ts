import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

import { AppRoutingModule } from 'app/app-routing.module';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { ChefComponentsModule } from 'app/components/chef-components.module';
import { ProjectDetailsComponent } from './details/project-details.component';
import { ProjectListComponent } from './list/project-list.component';
import {
  ConfirmApplyStartModalComponent
} from './confirm-apply-start-modal/confirm-apply-start-modal.component';
import {
  ConfirmApplyStopModalComponent
} from './confirm-apply-stop-modal/confirm-apply-stop-modal.component';
import { ProjectRulesComponent } from './rules/project-rules.component';

@NgModule({
  declarations: [
    ProjectDetailsComponent,
    ProjectListComponent,
    ConfirmApplyStartModalComponent,
    ConfirmApplyStopModalComponent,
    ProjectRulesComponent
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
export class ProjectModule {}
