import { NgModule } from '@angular/core';

import { CapitalizePipe } from './capitalize.pipe';
import { ChefStatusIconPipe } from './chef-status-icon.pipe';
import { DatetimePipe } from './datetime.pipe';
import { DefinedOrDefaultPipe } from './defined-or-default.pipe';
import { IamTypePipe } from './iam-type.pipe';
import { PluralizePipe } from './pluralize.pipe';
import { SelectListInputPipe } from './select-list-input.pipe';
import { SelectedStatusPipe } from './selected-status.pipe';
import { ServiceStatusIconPipe } from './service-status-icon.pipe';
import { StatusSelectorPipe } from './status-selector.pipe';
import { TimeDurationPipe } from './time-duration.pipe';
import { TimeFromNowPipe } from './time-from-now.pipe';

@NgModule({
  declarations: [
    CapitalizePipe,
    ChefStatusIconPipe,
    DatetimePipe,
    DefinedOrDefaultPipe,
    IamTypePipe,
    PluralizePipe,
    SelectListInputPipe,
    ServiceStatusIconPipe,
    SelectedStatusPipe,
    StatusSelectorPipe,
    TimeDurationPipe,
    TimeFromNowPipe
  ],
  exports: [
    CapitalizePipe,
    ChefStatusIconPipe,
    DatetimePipe,
    DefinedOrDefaultPipe,
    IamTypePipe,
    PluralizePipe,
    SelectListInputPipe,
    SelectedStatusPipe,
    ServiceStatusIconPipe,
    StatusSelectorPipe,
    TimeDurationPipe,
    TimeFromNowPipe
  ]
})
export class ChefPipesModule {}
