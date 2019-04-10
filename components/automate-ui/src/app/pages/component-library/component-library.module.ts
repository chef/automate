// angular modules

import { CommonModule } from '@angular/common';
import { NgModule } from '@angular/core';
import { RouterModule } from '@angular/router';

// chef modules

import { ChefComponentsModule } from '../../components/chef-components.module';
import { ClApiComponent } from './demos/cl-api/cl-api.component';
import { ClApiPropComponent } from './demos/cl-api-prop/cl-api-prop.component';
import { ClCodeSectionComponent } from './demos/cl-code-section/cl-code-section.component';
import { ClSectionComponent } from './demos/cl-section/cl-section.component';
import { ClTokenComponent } from './demos/cl-token/cl-token.component';
import {
  ComponentLibraryComponent
} from './component-library.component';
import {
  ComponentLibraryNavbarComponent
} from './demos/component-library-navbar/component-library-navbar.component';
import {
  ComponentLibraryRootComponent
} from './demos/component-library-root/component-library-root.component';
import { ComponentLibraryRoutingModule } from './component-library.routing';
import {
  ComponentLibraryScreenComponent
} from './demos/component-library-screen/component-library-screen.component';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';

// **********************************************
// Add import for new demo components here:
// **********************************************
import { DemoCalendarComponent } from './demos/demo-calendar/demo-calendar.component';
import { DemoCheckboxComponent } from './demos/demo-checkbox/demo-checkbox.component';
import { DemoFormFieldComponent } from './demos/demo-form-field/demo-form-field.component';
import { DemoInputComponent } from './demos/demo-input/demo-input.component';
import { DemoSidebarComponent } from './demos/demo-sidebar/demo-sidebar.component';
import {
  DemoProjectsDropdownComponent
} from './demos/demo-projects-dropdown/demo-projects-dropdown.component';

@NgModule({
  imports: [
    CommonModule,
    ComponentLibraryRoutingModule,
    ChefComponentsModule,
    RouterModule
  ],
// **********************************************
// Add declaration for new demo components here:
// **********************************************
  declarations: [
    ClApiComponent,
    ClApiPropComponent,
    ClCodeSectionComponent,
    ClSectionComponent,
    ClTokenComponent,
    ComponentLibraryComponent,
    ComponentLibraryNavbarComponent,
    ComponentLibraryRootComponent,
    ComponentLibraryScreenComponent,
    DemoCalendarComponent,
    DemoCheckboxComponent,
    DemoFormFieldComponent,
    DemoInputComponent,
    DemoSidebarComponent,
    DemoProjectsDropdownComponent
  ],
  providers: [
  ],
  schemas: [
    CUSTOM_ELEMENTS_SCHEMA
  ]
})

export class ComponentLibraryModule {}
