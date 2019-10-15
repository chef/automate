import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ReportingControlsComponent } from './reporting-controls.component';

const routes: Routes = [
  {
    path: '',
    component: ReportingControlsComponent
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
export class ReportingControlsRoutingModule {}
