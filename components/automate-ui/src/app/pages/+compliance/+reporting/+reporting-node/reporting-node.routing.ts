import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ReportingNodeComponent } from './reporting-node.component';

const routes: Routes = [
  {
    path: '',
    component: ReportingNodeComponent
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
export class ReportingNodeRoutingModule {}
