import { Component, Output, EventEmitter, OnInit, Input } from '@angular/core';
import { HistorySelection } from '../../helpers/history-selection/history-selection';
import { find } from 'lodash';

@Component({
  selector: 'app-date-selector',
  templateUrl: './date-selector.component.html',
  styleUrls: ['./date-selector.component.scss']
})
export class DateSelectorComponent implements OnInit {
  @Output() select = new EventEmitter();
  @Input() initialDateTerm: string;
  // available selectors
  dates: string[] = HistorySelection.ALL;
  // selected date
  selectedDate: any;
  // selected date in dropdown, not confirmed until apply or cancel is clicked
  inSelection: any;
  // indicates if the dropdown is open
  dropdownOpen = false;

  ngOnInit() {
    let date = this.dates[0];
    if (this.initialDateTerm) {
      const foundDateTerm: string = find(this.dates, (term) => this.initialDateTerm === term);

      if (foundDateTerm) {
        date = foundDateTerm;
      }
    }

    this.selectedDate = date;
    this.inSelection = this.selectedDate;
  }

  onSelect(date: any) {
    this.inSelection = date;
  }

  apply() {
    this.selectedDate = this.inSelection;
    this.select.emit(this.selectedDate);
    this.dropdownOpen = false;
  }

  cancel() {
    this.inSelection = this.selectedDate;
    this.dropdownOpen = false;
  }
}

