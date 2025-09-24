import { Component, HostBinding } from '@angular/core';

@Component({
  standalone: false,
  selector: 'chef-td',
  templateUrl: './table-cell.component.html',
  styleUrls: ['./table-cell.component.scss']
})
export class TableCellComponent {
  @HostBinding('attr.role') role = 'cell';
}
