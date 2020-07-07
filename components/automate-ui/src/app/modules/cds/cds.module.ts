import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';


import { CardTableComponent } from './card-table/card-table.component';
import { CardComponent } from './card/card.component';
import { CdsRoutingModule } from './cds-routing.module';
import { DashboardComponent } from './dashboard/dashboard.component';
import { EnableContentModalComponent } from './enable-content-modal/enable-content-modal.component';

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    CdsRoutingModule
  ],
  exports: [
    CardTableComponent,
    CardComponent,
    DashboardComponent,
    EnableContentModalComponent
  ],
  declarations: [
    CardTableComponent,
    CardComponent,
    DashboardComponent,
    EnableContentModalComponent
  ],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
})
export class CdsModule { }
