import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ScannerComponent } from './containers/scanner/scanner.component';
import { JobsListComponent } from './containers/jobs-list/jobs-list.component';
import { JobScansListComponent } from './containers/job-scans-list/job-scans-list.component';
import { NodesListComponent } from './containers/nodes-list/nodes-list.component';
import { NodesAddComponent } from './containers/nodes-add/nodes-add.component';
import { NodesEditComponent } from './containers/nodes-edit/nodes-edit.component';

const routes: Routes = [
  {
    path: '',
    component: ScannerComponent,
    children: [
      {
        path: '',
        redirectTo: 'jobs',
        pathMatch: 'full'
      },
      {
        path: 'jobs',
        component: JobsListComponent
      },
      {
        path: 'nodes',
        component: NodesListComponent
      }
    ]
  },
  {
    path: 'jobs/:id/scans',
    component: JobScansListComponent
  },
  {
    path: 'nodes/add',
    component: NodesAddComponent
  },
  {
    path: 'nodes/:id/edit',
    component: NodesEditComponent
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
export class ScannerRoutingModule {}
