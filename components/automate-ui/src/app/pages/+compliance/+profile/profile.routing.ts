import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ProfileDetailsComponent } from './+profile-details/profile-details.component';
import { ProfileOverviewComponent } from './+profile-overview/profile-overview.component';

const routes: Routes = [
  {
    path: '',
    component: ProfileOverviewComponent
  },
  {
    path: 'profile-details',
    component: ProfileDetailsComponent
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
export class ProfileRoutingModule {}
