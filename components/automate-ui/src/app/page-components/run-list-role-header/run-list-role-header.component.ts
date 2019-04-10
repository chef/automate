import { Component, Input } from '@angular/core';
import { Item } from '../run-list/run-list.component';

@Component({
  selector: 'app-run-list-role-header',
  templateUrl: './run-list-role-header.component.html',
  styleUrls: ['./run-list-role-header.component.scss']
})
export class RunListRoleHeaderComponent {
  @Input() roles: Item;
  expanded = false;

  toggleExpand() {
    this.expanded = !this.expanded;
  }
}
