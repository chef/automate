import { Component, HostBinding } from '@angular/core';

@Component({
  selector: 'chef-table-header',
  templateUrl: './table-header.component.html'
})
export class TableHeaderComponent {
  @HostBinding('attr.role') role = 'rowgroup';
}
