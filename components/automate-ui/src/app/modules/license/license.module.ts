// Modules
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { RouterModule } from '@angular/router';
import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';

import {LicenseApplyComponent} from './license-apply/license-apply.component';
import {LicenseLockoutComponent} from './license-lockout/license-lockout.component';
import {LicenseNotificationsComponent} from './license-notifications/license-notifications.component';

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    RouterModule
  ],
  exports: [
    LicenseApplyComponent,
    LicenseLockoutComponent,
    LicenseNotificationsComponent
  ],
  declarations: [
    LicenseApplyComponent,
    LicenseLockoutComponent,
    LicenseNotificationsComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class ChefLicenseModule { }
