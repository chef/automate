import { Component, HostBinding } from '@angular/core';

@Component({
  selector: 'chef-thead',
  templateUrl: './table-header.component.html'
})
export class TableHeaderComponent {
  @HostBinding('attr.role') role = 'rowgroup';
}
