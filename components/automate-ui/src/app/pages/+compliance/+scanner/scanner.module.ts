import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { ReactiveFormsModule } from '@angular/forms';
import { ChefComponentsModule } from '../../../components/chef-components.module';
import { ComplianceSharedModule } from '../shared/shared.module';
import { ChefStatusIconPipe } from '../../../pipes/chef-status-icon.pipe';
import { ProfilesService } from '../../../services/profiles/profiles.service';
import { ScannerRoutingModule } from './scanner.routing';
import { ScannerComponent } from './containers/scanner/scanner.component';
import { JobsListComponent } from './containers/jobs-list/jobs-list.component';
import { JobScansListComponent } from './containers/job-scans-list/job-scans-list.component';
import { NodesListComponent } from './containers/nodes-list/nodes-list.component';
import { NodesAddComponent } from './containers/nodes-add/nodes-add.component';
import { NodesEditComponent } from './containers/nodes-edit/nodes-edit.component';
import { AccordionComponent } from '../../../page-components/accordion/accordion.component';
import { AccordionItemComponent } from '../../../page-components/accordion/accordion-item/accordion-item.component';

@NgModule({
  imports: [
    CommonModule,
    ChefComponentsModule,
    ComplianceSharedModule,
    ReactiveFormsModule,
    RouterModule,
    ScannerRoutingModule
  ],
  providers: [
    ChefStatusIconPipe,
    ProfilesService
  ],
  declarations: [
    ScannerComponent,
    JobsListComponent,
    JobScansListComponent,
    NodesListComponent,
    NodesAddComponent,
    NodesEditComponent,
    AccordionComponent,
    AccordionItemComponent
  ],
  schemas: [
    CUSTOM_ELEMENTS_SCHEMA
  ]
})
export class ScannerModule {}
