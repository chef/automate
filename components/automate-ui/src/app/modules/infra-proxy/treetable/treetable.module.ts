import { CommonModule } from '@angular/common';
import { NgModule } from '@angular/core';
import { MatTableModule } from '@angular/material/table';
import { MatIconModule } from '@angular/material/icon';
import { TreetableComponent } from './component/treetable.component';
export { Node, Options } from './models';

@NgModule({
  declarations: [
    TreetableComponent
  ],
  imports: [
    CommonModule,
    MatTableModule,
    MatIconModule
  ],
  exports: [
    TreetableComponent
  ]
})
export class TreetableModule { }
