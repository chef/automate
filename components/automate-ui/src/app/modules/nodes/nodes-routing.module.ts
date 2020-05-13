import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { NodesListComponent } from './nodes-list/nodes-list.component';

const nodesRoutes: Routes = [
  {
    path: '',
    component: NodesListComponent
  }
];

@NgModule({
  imports: [
    RouterModule.forChild(nodesRoutes)
  ],
  exports: [
    RouterModule
  ]
})
export class NodesRoutingModule { }
