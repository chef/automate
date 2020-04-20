import { Component, HostBinding } from '@angular/core';

@Component({
  selector: 'chef-tr',
  templateUrl: './table-row.component.html',
  styleUrls: ['./table-row.component.scss']
})
export class TableRowComponent {
  @HostBinding('attr.role') role = 'row';
}
