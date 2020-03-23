import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { DailyCheckInComponent } from './daily-check-in/daily-check-in.component';

const desktopRoutes: Routes = [
  {
    path: '',
    component: DailyCheckInComponent
  }
];

@NgModule({
  imports: [
    RouterModule.forChild(desktopRoutes)
  ],
  exports: [
    RouterModule
  ]
})
export class DesktopRoutingModule { }
