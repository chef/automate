import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';

import {
    TelemetryCheckboxComponent
  } from './telemetry-checkbox.component';

@NgModule({
  imports: [
    CommonModule
  ],
  declarations: [
    TelemetryCheckboxComponent
  ],
  exports: [
    TelemetryCheckboxComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class TelemetryCheckboxModule { }
