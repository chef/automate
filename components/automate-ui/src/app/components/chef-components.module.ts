// Modules
import { ChefPipesModule } from '../pipes/chef-pipes.module';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { RouterModule } from '@angular/router';
import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { MatDialogModule } from '@angular/material/dialog';
import { MatSnackBarModule } from '@angular/material/snack-bar';
import { MatSelectModule  } from '@angular/material/select';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { MatButtonModule } from '@angular/material/button';

// Components
import { AuthorizedComponent } from './authorized/authorized.component';
import { BreadcrumbsComponent } from './breadcrumbs/breadcrumbs.component';
import { BreadcrumbComponent } from './breadcrumb/breadcrumb.component';
import { CalendarComponent } from './calendar/calendar.component';
import { ChefNotificationsComponent } from './notifications/notifications.component';
import { ClientRunsSidebarComponent } from './client-runs-sidebar/client-runs-sidebar.component';
import { ConfirmApplyStartModalComponent } from './confirm-apply-start-modal/confirm-apply-start-modal.component';
import { ConfirmApplyStopModalComponent } from './confirm-apply-stop-modal/confirm-apply-stop-modal.component';
import { CreateObjectModalComponent } from './create-object-modal/create-object-modal.component';
import { CreateUserModalComponent } from 'app/page-components/create-user-modal/create-user-modal.component';
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
import { MessageModalComponent } from './message-modal/message-modal.component';
import { PagePickerComponent } from './page-picker/page-picker.component';
import { PendingEditsBarComponent } from './pending-edits-bar/pending-edits-bar.component';
import { ProcessProgressBarComponent } from './process-progress-bar/process-progress-bar.component';
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

@NgModule({
  imports: [
    ChefPipesModule,
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    RouterModule,

    // Angular Material
    MatDialogModule,
    MatSelectModule,
    MatSnackBarModule,
    MatProgressBarModule,
    MatButtonModule
  ],
  exports: [
    // Angular Material
    MatDialogModule,
    MatSelectModule,
    MatSnackBarModule,
    MatProgressBarModule,
    MatButtonModule,

    // Components
    AuthorizedComponent,
    BreadcrumbsComponent,
    BreadcrumbComponent,
    CalendarComponent,
    ChefNotificationsComponent,
    ClientRunsSidebarComponent,
    ConfirmApplyStartModalComponent,
    ConfirmApplyStopModalComponent,
    CreateObjectModalComponent,
    CreateUserModalComponent,
    DeleteObjectModalComponent,
    FormFieldComponent,
    GuitarStringComponent,
    GuitarStringCollectionComponent,
    GuitarStringItemComponent,
    GuitarStringSectionComponent,
    LandingComponent,
    MessageModalComponent,
    PagePickerComponent,
    PendingEditsBarComponent,
    ProcessProgressBarComponent,
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
    InputDirective
  ],
  declarations: [
    AuthorizedComponent,
    BreadcrumbsComponent,
    BreadcrumbComponent,
    CalendarComponent,
    ChefNotificationsComponent,
    ClientRunsSidebarComponent,
    ConfirmApplyStartModalComponent,
    ConfirmApplyStopModalComponent,
    CreateObjectModalComponent,
    CreateUserModalComponent,
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
    MessageModalComponent,
    PagePickerComponent,
    PendingEditsBarComponent,
    ProcessProgressBarComponent,
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
    TableHeaderComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class ChefComponentsModule { }
