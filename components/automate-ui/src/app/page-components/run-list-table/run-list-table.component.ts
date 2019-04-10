import { Component, Input } from '@angular/core';
import { Item } from '../run-list/run-list.component';

@Component({
  selector: 'app-run-list-table',
  templateUrl: './run-list-table.component.html',
  styleUrls: ['./run-list-table.component.scss']
})
export class RunListTableComponent {
  @Input() item: Item;
}
