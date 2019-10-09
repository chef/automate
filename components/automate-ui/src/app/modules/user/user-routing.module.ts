import { NgModule }             from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { UserDetailsComponent } from './user-details/user-details.component';
import { UserManagementComponent } from './user-management/user-management.component';
import { UserDetailsNonAdminResolve } from './user-details/user-details.resolver';

const userRoutes: Routes = [
  {
    path: '',
    component: UserManagementComponent
  },
  {
    path: ':id',
    component: UserDetailsComponent
  },
  {
    path: 'user-details/:id',
    component: UserDetailsComponent,
    resolve: { isNonAdmin: UserDetailsNonAdminResolve }
  }
];

@NgModule({
  imports: [
    RouterModule.forChild(userRoutes)
  ],
  exports: [
    RouterModule
  ]
})
export class UserRoutingModule { }
