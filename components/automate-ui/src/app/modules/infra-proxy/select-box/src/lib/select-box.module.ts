import { NgModule } from '@angular/core';
import { SelectBoxComponent } from './select-box.component';
import { ListFilterPipe } from './list-filter.pipe';
import { DragDropModule } from '@angular/cdk/drag-drop';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { BrowserModule } from '@angular/platform-browser';
import { ChefComponentsModule } from 'app/components/chef-components.module';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';



@NgModule({
  imports: [
    BrowserModule,
    FormsModule,
    ReactiveFormsModule,
    DragDropModule,
    ChefComponentsModule,
    ChefPipesModule,
    MatIconModule,
    MatButtonModule
  ],
  declarations: [SelectBoxComponent, ListFilterPipe],
  exports: [SelectBoxComponent, ListFilterPipe]
})
export class SelectBoxModule { }
