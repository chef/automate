import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import {
  FormsModule,
  ReactiveFormsModule
} from '@angular/forms';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';

import { ChefComponentsModule } from 'app/components/chef-components.module';
import { ComplianceSharedModule } from '../shared/shared.module';
import { NodeCredentialsRoutingModule } from './node-credentials.routing';
import {
  NodeCredentialListComponent } from './node-credentials-list/node-credential-list.component';
import { NodeCredentialDetailsScreenComponent } from './node-credential-details/node-credential-details.component';
import { CreateNodeCredentialModalComponent } from './create-node-credential-modal/create-node-credential-modal.component';
import { SaveNodeCredential } from 'app/entities/node-credentials/node-credential.model';
import { InfiniteScrollModule } from 'ngx-infinite-scroll';
@NgModule({
  imports: [
    CommonModule,
    ChefComponentsModule,
    ComplianceSharedModule,
    NodeCredentialsRoutingModule,
    FormsModule,
    ReactiveFormsModule,
    RouterModule,
    InfiniteScrollModule
  ],
  declarations: [
    NodeCredentialListComponent,
    NodeCredentialDetailsScreenComponent,
    CreateNodeCredentialModalComponent
  ],
  providers: [SaveNodeCredential],
  schemas: [CUSTOM_ELEMENTS_SCHEMA]
})
export class NodeCredentialsModule {}
