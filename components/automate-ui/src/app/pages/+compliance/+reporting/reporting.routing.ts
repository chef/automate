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
        loadChildren: () => import('./+reporting-overview/reporting-overview.module')
          .then(m => m.ReportingOverviewModule)
      },
      {
        path: 'nodes',
        loadChildren: () => import('./+reporting-nodes/reporting-nodes.module')
          .then(m => m.ReportingNodesModule)
      },
      {
        path: 'profiles',
        loadChildren: () => import('./+reporting-profiles/reporting-profiles.module')
          .then(m => m.ReportingProfilesModule)
      },
      {
        path: 'controls',
        loadChildren: () => import('./+reporting-controls/reporting-controls.module')
          .then(m => m.ReportingControlsModule)
      }
    ]
  },
  {
    path: 'nodes/:id',
    loadChildren: () => import('./+reporting-node/reporting-node.module')
      .then(m => m.ReportingNodeModule)
  },
  {
    path: 'profiles/:id',
    loadChildren: () => import('./+reporting-profile/reporting-profile.module')
      .then(m => m.ReportingProfileModule)
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
