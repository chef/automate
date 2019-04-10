// Modules
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { RouterModule } from '@angular/router';
import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { MatDialogModule, MatSnackBarModule } from '@angular/material';

// Components
import { AdminSidebarComponent } from './admin-sidebar/admin-sidebar.component';
import { AuthorizedComponent } from './authorized/authorized.component';
import { BreadcrumbsComponent } from './breadcrumbs/breadcrumbs.component';
import { BreadcrumbComponent } from './breadcrumb/breadcrumb.component';
import { CalendarComponent } from './calendar/calendar.component';
import { CreateObjectModalComponent } from './create-object-modal/create-object-modal.component';
import { DeleteObjectModalComponent } from './delete-object-modal/delete-object-modal.component';
import { ErrorDirective } from './error/error.directive';
import { FormFieldComponent } from './form-field/form-field.component';
import {
  GuitarStringComponent
} from './guitar-string-collection/guitar-string/guitar-string.component';
import {
  GuitarStringCollectionComponent
} from './guitar-string-collection/guitar-string-collection.component';
import {
  GuitarStringItemComponent
} from './guitar-string-collection/guitar-string-item/guitar-string-item.component';
import {
  GuitarStringSectionComponent
} from './guitar-string-collection/guitar-string-section/guitar-string-section.component';
import { InputDirective } from './input/input.directive';
import { LandingComponent } from './landing/landing.component';
import { PagePickerComponent } from './page-picker/page-picker.component';
import { ProjectsDropdownComponent } from './projects-dropdown/projects-dropdown.component';
import { SidebarComponent } from './sidebar/sidebar.component';
import { SidebarNoShadeComponent } from './sidebar-no-shade/sidebar-no-shade.component';
import { SidebarEntryComponent } from './sidebar-entry/sidebar-entry.component';
import { TabComponent } from './tab/tab.component';
import { TabsComponent } from './tabs/tabs.component';

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    RouterModule,

    // Angular Material
    MatDialogModule,
    MatSnackBarModule
  ],
  exports: [
    // Angular Material
    MatDialogModule,
    MatSnackBarModule,

    // Components
    AdminSidebarComponent,
    AuthorizedComponent,
    BreadcrumbsComponent,
    BreadcrumbComponent,
    CalendarComponent,
    CreateObjectModalComponent,
    DeleteObjectModalComponent,
    FormFieldComponent,
    GuitarStringComponent,
    GuitarStringCollectionComponent,
    GuitarStringItemComponent,
    GuitarStringSectionComponent,
    LandingComponent,
    PagePickerComponent,
    ProjectsDropdownComponent,
    SidebarComponent,
    SidebarNoShadeComponent,
    SidebarEntryComponent,
    TabComponent,
    TabsComponent,

    // Directives
    ErrorDirective,
    InputDirective
  ],
  declarations: [
    AdminSidebarComponent,
    AuthorizedComponent,
    BreadcrumbsComponent,
    BreadcrumbComponent,
    CalendarComponent,
    CreateObjectModalComponent,
    DeleteObjectModalComponent,
    ErrorDirective,
    FormFieldComponent,
    GuitarStringComponent,
    GuitarStringCollectionComponent,
    GuitarStringItemComponent,
    GuitarStringSectionComponent,
    InputDirective,
    LandingComponent,
    PagePickerComponent,
    ProjectsDropdownComponent,
    SidebarComponent,
    SidebarNoShadeComponent,
    SidebarEntryComponent,
    TabComponent,
    TabsComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class ChefComponentsModule { }
