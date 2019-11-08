// Modules
import { ChefPipesModule } from '../pipes/chef-pipes.module';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { RouterModule } from '@angular/router';
import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { MatDialogModule } from '@angular/material/dialog';
import { MatSnackBarModule } from '@angular/material/snack-bar';

// Components
import { AuthorizedComponent } from './authorized/authorized.component';
import { BreadcrumbsComponent } from './breadcrumbs/breadcrumbs.component';
import { BreadcrumbComponent } from './breadcrumb/breadcrumb.component';
import { CalendarComponent } from './calendar/calendar.component';
import { ChefControlMenuComponent } from './chef-control-menu/chef-control-menu.component';
import { CreateObjectModalComponent } from './create-object-modal/create-object-modal.component';
import { DeleteObjectModalComponent } from './delete-object-modal/delete-object-modal.component';
import { ErrorDirective } from './error/error.directive';
import { FormControlDirective } from './form-control/form-control.directive';
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
import { SettingsSidebarComponent } from './settings-sidebar/settings-sidebar.component';
import { SidebarComponent } from './sidebar/sidebar.component';
import { SidebarNoShadeComponent } from './sidebar-no-shade/sidebar-no-shade.component';
import { SidebarEntryComponent } from './sidebar-entry/sidebar-entry.component';
import { TabComponent } from './tab/tab.component';
import { TabsComponent } from './tabs/tabs.component';
import { TableBodyComponent } from './table/table-body/table-body.component';
import { TableComponent } from './table/table.component';
import { TableCellComponent } from './table/table-cell/table-cell.component';
import { TableRowComponent } from './table/table-row/table-row.component';
import { TableHeaderCellComponent } from './table/table-header-cell/table-header-cell.component';
import { TableHeaderComponent } from './table/table-header/table-header.component';
import { ChefOptionComponent } from './chef-option/chef-option.component';
import { ChefDropdownComponent } from './chef-dropdown/chef-dropdown.component';

@NgModule({
  imports: [
    ChefPipesModule,
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
    AuthorizedComponent,
    BreadcrumbsComponent,
    BreadcrumbComponent,
    CalendarComponent,
    ChefControlMenuComponent,
    ChefOptionComponent,
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
    SettingsSidebarComponent,
    SidebarComponent,
    SidebarNoShadeComponent,
    SidebarEntryComponent,
    TabComponent,
    TabsComponent,
    TableBodyComponent,
    TableComponent,
    TableCellComponent,
    TableRowComponent,
    TableHeaderCellComponent,
    TableHeaderComponent,

    // Directives
    ErrorDirective,
    FormControlDirective,
    InputDirective,
    ChefDropdownComponent
  ],
  declarations: [
    AuthorizedComponent,
    BreadcrumbsComponent,
    BreadcrumbComponent,
    CalendarComponent,
    CreateObjectModalComponent,
    DeleteObjectModalComponent,
    ErrorDirective,
    FormControlDirective,
    FormFieldComponent,
    GuitarStringComponent,
    GuitarStringCollectionComponent,
    GuitarStringItemComponent,
    GuitarStringSectionComponent,
    InputDirective,
    LandingComponent,
    PagePickerComponent,
    ProjectsDropdownComponent,
    SettingsSidebarComponent,
    SidebarComponent,
    SidebarNoShadeComponent,
    SidebarEntryComponent,
    TabComponent,
    TabsComponent,
<<<<<<< HEAD
<<<<<<< HEAD
    TableBodyComponent,
    TableComponent,
    TableCellComponent,
    TableRowComponent,
    TableHeaderCellComponent,
    TableHeaderComponent
=======
    ChefControlMenuComponent
>>>>>>> Generated new chef-control-menu component and dropped it into the chefComponentsModule, scss now connected.  Logic not yet built
=======
    ChefControlMenuComponent,
    ChefOptionComponent,
    ChefDropdownComponent
>>>>>>> WIP - added chef option and dropdown angular components
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class ChefComponentsModule { }
