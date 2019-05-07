import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ComplianceComponent } from './compliance.component';

const routes: Routes = [
  {
    path: '',
    component: ComplianceComponent,
    children: [
      {
        path: '',
        redirectTo: 'reports',
        pathMatch: 'full'
      },
      {
        path: 'reporting',
        loadChildren: './+reporting/reporting.module#ReportingModule'
      },
      {
        path: 'reports',
        loadChildren: './+reporting/reporting.module#ReportingModule'
      },
      {
        path: 'scanner',
        loadChildren: './+scanner/scanner.module#ScannerModule'
      },
      {
        path: 'scan-jobs',
        loadChildren: './+scanner/scanner.module#ScannerModule'
      },
      {
        path: 'profiles',
        loadChildren: './+profile/profile.module#ProfileModule'
      }
    ]
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
export class ComplianceRoutingModule {}
