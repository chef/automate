import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ReportingProfileComponent } from './reporting-profile.component';

const routes: Routes = [
  {
    path: '',
    component: ReportingProfileComponent
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
export class ReportingProfileRoutingModule {}
