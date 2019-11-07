import { Component, HostBinding } from '@angular/core';

@Component({
  selector: 'chef-table-body',
  templateUrl: './table-body.component.html',
  styleUrls: ['./table-body.component.scss']
})
export class TableBodyComponent {
  @HostBinding('attr.role') role = 'rowgroup';
}
