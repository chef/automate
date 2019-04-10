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
        redirectTo: 'reporting',
        pathMatch: 'full'
      },
      {
        path: 'reporting',
        loadChildren: './+reporting/reporting.module#ReportingModule'
      },
      {
        path: 'scanner',
        loadChildren: './+scanner/scanner.module#ScannerModule'
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
