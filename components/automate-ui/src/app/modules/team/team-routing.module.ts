import { NgModule }             from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { TeamManagementComponent } from './team-management/team-management.component';
import { TeamDetailsComponent } from './team-details/team-details.component';
import { TeamAddUsersComponent } from './team-add-users/team-add-users.component';

const teamRoutes: Routes = [
    {
        path: '',
        component: TeamManagementComponent
    },
    {
        path: ':id',
        component: TeamDetailsComponent
    },
    {
        path: ':id/add-users',
        component: TeamAddUsersComponent,
        data: { hideNavBar: true }
    },
];

@NgModule({
  imports: [
    RouterModule.forChild(teamRoutes)
  ],
  exports: [
    RouterModule
  ]
})
export class TeamRoutingModule { }
