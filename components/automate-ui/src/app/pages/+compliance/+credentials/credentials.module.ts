import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import {
  FormsModule,
  ReactiveFormsModule
} from '@angular/forms';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';

import { ChefComponentsModule } from '../../../components/chef-components.module';
import { ComplianceSharedModule } from '../shared/shared.module';
import { CredentialsRoutingModule } from './credentials.routing';
import { CredentialsLogic } from './credentials.logic';
import { CredentialsListScreenComponent } from './containers/credentials-list-screen';
import { CredentialsAddEditScreenComponent } from './containers/credentials-add-edit-screen';
import { CredentialsListComponent } from './components/credentials-list';
import { CredentialsListRowComponent } from './components/credentials-list-row';
import { CredentialsFormComponent } from './components/credentials-form';

@NgModule({
  imports: [
    CommonModule,
    ChefComponentsModule,
    ComplianceSharedModule,
    CredentialsRoutingModule,
    FormsModule,
    ReactiveFormsModule,
    RouterModule
  ],
  declarations: [
    CredentialsListScreenComponent,
    CredentialsAddEditScreenComponent,
    CredentialsListComponent,
    CredentialsListRowComponent,
    CredentialsFormComponent
  ],
  providers: [
    CredentialsLogic
  ],
  schemas: [
    CUSTOM_ELEMENTS_SCHEMA
  ]
})
export class CredentialsModule {}
