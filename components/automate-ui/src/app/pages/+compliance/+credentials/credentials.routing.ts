import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { CredentialsListScreenComponent } from './containers/credentials-list-screen';
import { CredentialsAddEditScreenComponent } from './containers/credentials-add-edit-screen';

const routes: Routes = [
  {
    path: '',
    component: CredentialsListScreenComponent
  },
  {
    path: 'add',
    component: CredentialsAddEditScreenComponent
  },
  {
    path: ':id/edit',
    component: CredentialsAddEditScreenComponent
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
export class CredentialsRoutingModule {}
