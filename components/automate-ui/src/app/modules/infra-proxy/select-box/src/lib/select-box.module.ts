import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule } from '@angular/common';

import { SelectBoxComponent } from './select-box.component';
import { DragDropModule } from '@angular/cdk/drag-drop';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { BrowserModule } from '@angular/platform-browser';
import { ChefComponentsModule } from 'app/components/chef-components.module';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { ScrollingModule } from '@angular/cdk/scrolling';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';

@NgModule({
  imports: [
    CommonModule,
    BrowserModule,
    FormsModule,
    ReactiveFormsModule,
    DragDropModule,
    ChefComponentsModule,
    ChefPipesModule,
    MatIconModule,
    MatButtonModule,
    ScrollingModule,
    MatProgressSpinnerModule
 ],
 schemas: [ CUSTOM_ELEMENTS_SCHEMA ],

  declarations: [SelectBoxComponent],
  exports: [SelectBoxComponent]
})
export class SelectBoxModule { }
