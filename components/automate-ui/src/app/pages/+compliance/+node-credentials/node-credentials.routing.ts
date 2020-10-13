import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import {
  NodeCredentialListComponent } from './node-credentials-list/node-credential-list.component';
import { NodeCredentialDetailsScreenComponent } from './node-credential-details/node-credential-details.component';
import { CreateNodeCredentialModalComponent } from './create-node-credential-modal/create-node-credential-modal.component';

const routes: Routes = [
  {
    path: '',
    component: NodeCredentialListComponent
  },
  {
    path: 'add',
    component: CreateNodeCredentialModalComponent
  },
  {
    path: ':id/edit',
    component: NodeCredentialDetailsScreenComponent
  }
];

@NgModule({
  imports: [
    RouterModule.forChild(routes)
  ],
  exports: [
    RouterModule
  ]
})
export class NodeCredentialsRoutingModule {}
