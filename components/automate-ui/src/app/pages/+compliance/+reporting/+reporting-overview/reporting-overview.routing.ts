import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ReportingOverviewComponent } from './reporting-overview.component';

const routes: Routes = [
  {
    path: '',
    component: ReportingOverviewComponent
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
export class ReportingOverviewRoutingModule {}
