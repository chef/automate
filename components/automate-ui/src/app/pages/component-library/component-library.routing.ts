// modules

import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

// infrastructure components

import { ComponentLibraryComponent } from './component-library.component';
import {
  ComponentLibraryRootComponent
} from './demos/component-library-root/component-library-root.component';

// components

import { DemoCalendarComponent } from './demos/demo-calendar/demo-calendar.component';
import { DemoCheckboxComponent } from './demos/demo-checkbox/demo-checkbox.component';
import { DemoFormFieldComponent } from './demos/demo-form-field/demo-form-field.component';
import { DemoInputComponent } from './demos/demo-input/demo-input.component';
import { DemoSidebarComponent } from './demos/demo-sidebar/demo-sidebar.component';
import {
  DemoProjectsDropdownComponent
} from './demos/demo-projects-dropdown/demo-projects-dropdown.component';

const routes: Routes = [
  {
    path: '',
    component: ComponentLibraryComponent,
    children: [
      {
        path: '',
        component: ComponentLibraryRootComponent
      },
      {
        path: 'calendar',
        component: DemoCalendarComponent
      },
      {
        path: 'checkbox',
        component: DemoCheckboxComponent
      },
      {
        path: 'form-field',
        component: DemoFormFieldComponent
      },
      {
        path: 'input',
        component: DemoInputComponent
      },
      {
        path: 'sidebar',
        component: DemoSidebarComponent
      },
      {
        path: 'projects-dropdown',
        component: DemoProjectsDropdownComponent
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
export class ComponentLibraryRoutingModule {}
