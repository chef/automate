import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ReportingNodesComponent } from './reporting-nodes.component';

const routes: Routes = [
  {
    path: '',
    component: ReportingNodesComponent
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
export class ReportingNodesRoutingModule {}
