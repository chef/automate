import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ReportingProfilesComponent } from './reporting-profiles.component';

const routes: Routes = [
  {
    path: '',
    component: ReportingProfilesComponent
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
export class ReportingProfilesRoutingModule {}
