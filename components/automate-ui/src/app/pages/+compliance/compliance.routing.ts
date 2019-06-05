import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ComplianceComponent } from './compliance.component';
import {
  ComplianceLandingComponent
} from 'app/pages/compliance-landing/compliance-landing.component';

const routes: Routes = [
  {
    path: '',
    component: ComplianceComponent,
    children: [
      {
        path: '',
        pathMatch: 'full',
        component: ComplianceLandingComponent
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
        path: 'compliance-profiles',
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
