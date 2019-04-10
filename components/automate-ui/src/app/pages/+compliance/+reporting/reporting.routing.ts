import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ReportingComponent } from './reporting.component';

const routes: Routes = [
  {
    path: '',
    component: ReportingComponent,
    children: [
      {
        path: '',
        redirectTo: 'overview',
        pathMatch: 'full'
      },
      {
        path: 'overview',
        loadChildren: './+reporting-overview/reporting-overview.module#ReportingOverviewModule'
      },
      {
        path: 'nodes',
        loadChildren: './+reporting-nodes/reporting-nodes.module#ReportingNodesModule'
      },
      {
        path: 'profiles',
        loadChildren: './+reporting-profiles/reporting-profiles.module#ReportingProfilesModule'
      }
    ]
  },
  {
    path: 'nodes/:id',
    loadChildren: './+reporting-node/reporting-node.module#ReportingNodeModule'
  },
  {
    path: 'profiles/:id',
    loadChildren: './+reporting-profile/reporting-profile.module#ReportingProfileModule'
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
export class ReportingRoutingModule {}
