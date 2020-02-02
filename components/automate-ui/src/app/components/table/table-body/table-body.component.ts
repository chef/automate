import { Component, HostBinding } from '@angular/core';

@Component({
  selector: 'chef-table-body',
  templateUrl: './table-body.component.html'
})
export class TableBodyComponent {
  @HostBinding('attr.role') role = 'rowgroup';
}
