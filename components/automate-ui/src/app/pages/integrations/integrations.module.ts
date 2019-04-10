import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

import { ChefComponentsModule } from 'app/components/chef-components.module';
import { AppRoutingModule } from 'app/app-routing.module';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';

import { IntegrationsAddComponent } from './add/integrations-add.component';
import { IntegrationsAWSFormComponent } from './aws-form/aws-form.component';
import { IntegrationsAzureFormComponent } from './azure-form/azure-form.component';
import { IntegrationsDetailComponent } from './detail/integrations-detail.component';
import { IntegrationsEditComponent } from './edit/integrations-edit.component';
import { IntegrationsFormComponent } from './integrations-form/integrations-form.component';
import { IntegrationsGCPFormComponent } from './gcp-form/gcp-form.component';
import { IntegrationsListComponent } from './list/integrations-list.component';

@NgModule({
  declarations: [
    IntegrationsAddComponent,
    IntegrationsAWSFormComponent,
    IntegrationsAzureFormComponent,
    IntegrationsDetailComponent,
    IntegrationsEditComponent,
    IntegrationsFormComponent,
    IntegrationsGCPFormComponent,
    IntegrationsListComponent
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
export class IntegrationsModule {}
