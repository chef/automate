import { Component, HostBinding } from '@angular/core';

@Component({
  selector: 'chef-table-cell',
  templateUrl: './table-cell.component.html',
  styleUrls: ['./table-cell.component.scss']
})
export class TableCellComponent {
  @HostBinding('attr.role') role = 'cell';
}
