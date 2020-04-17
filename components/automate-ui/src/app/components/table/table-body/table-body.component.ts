import { Component, HostBinding } from '@angular/core';

@Component({
  selector: 'chef-tbody',
  templateUrl: './table-body.component.html'
})
export class TableBodyComponent {
  @HostBinding('attr.role') role = 'rowgroup';
}
