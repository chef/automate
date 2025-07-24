import { Component, Input } from '@angular/core';

/* Small display table with no header where text in
   first row is bolded. */
@Component({
  standalone: false,
  selector: 'app-mini-table',
  templateUrl: './mini-table.component.html',
  styleUrls: ['./mini-table.component.scss']
})
export class MiniTableComponent {
  @Input() tableData: Array<Array<String>> = [[]];

  constructor() { }
}
