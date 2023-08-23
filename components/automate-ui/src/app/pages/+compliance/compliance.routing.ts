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
        loadChildren: () => import('./+reporting/reporting.module').then(m => m.ReportingModule)
      },
      {
        path: 'reports',
        loadChildren: () => import('./+reporting/reporting.module').then(m => m.ReportingModule)
      },
      {
        path: 'scanner',
        loadChildren: () => import('./+scanner/scanner.module').then(m => m.ScannerModule)
      },
      {
        path: 'scan-jobs',
        loadChildren: () => import('./+scanner/scanner.module').then(m => m.ScannerModule)
      },
      {
        path: 'compliance-profiles',
        loadChildren: () => import('./+profile/profile.module').then(m => m.ProfileModule)
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
