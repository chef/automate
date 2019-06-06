import { Component, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';

@Component({
  selector: 'app-reporting-searchbar',
  templateUrl: './reporting-searchbar.component.html',
  styleUrls: ['./reporting-searchbar.component.scss']
})
export class ReportingSearchbarComponent implements OnInit {
  @Input() date = new Date();
  @Input() filters = [];
  @Input() filterTypes = [];
  @Input() filterValues = [];

  @Output() suggestValues = new EventEmitter();
  @Output() filtersCleared = new EventEmitter();
  @Output() filterRemoved = new EventEmitter();
  @Output() filterAdded = new EventEmitter();
  @Output() dateChanged = new EventEmitter();

  calendarVisible = false;
  keyInputVisible = true;
  valInputVisible = false;
  filtersVisible = true;
  visibleDate;
  selectedType;

  @ViewChild('keyInput') keyInput;
  @ViewChild('valInput') valInput;

  ngOnInit() {
    this.visibleDate = new Date(this.date);
  }

  toggleFilters() {
    this.filtersVisible = !this.filtersVisible;
  }

  hideFilters() {
    this.filtersVisible = false;
  }

  showFilters() {
    this.filtersVisible = true;
  }

  toggleCalendar() {
    this.calendarVisible = !this.calendarVisible;
  }

  hideCalendar() {
    this.calendarVisible = false;
  }

  showCalendar() {
    this.calendarVisible = true;
  }

  onKeyChange(e) {
    const type = this.filterTypes.filter(t => t.title === e.target.value)[0];
    const text = '';

    if (type) {
      this.selectedType = type;
      this.suggestValues.emit({ detail: { type: type.name, text }});

      this.keyInputVisible = false;
      this.valInputVisible = true;

      setTimeout(() => {
        this.valInput.nativeElement.focus();
      }, 10);
    }
  }

  onValKeydown(e) {
    const text = e.target.value;

    if (e.keyCode === 8 && text.length === 0) {
      this.selectedType = null;

      this.keyInputVisible = true;
      this.valInputVisible = false;

      setTimeout(() => {
        this.keyInput.nativeElement.value = '';
        this.keyInput.nativeElement.focus();
      }, 10);
    }
  }

  onValInput(e) {
    const type = this.selectedType.name;
    const text = e.target.value;

    if (text.length > 0) {
      this.suggestValues.emit({ detail: { type, text }});
    }
  }

  onValChange(e) {
    const type = this.selectedType;
    const text: string = e.target.value;
    const suggestionValue = this.filterValues.filter(v => v.title === text)[0];

    if (text.indexOf('?') >= 0 || text.indexOf('*') >= 0) {
      const wildcardValue = {text, score: 0, version: '', title: text};

      this.addNewFilter(type, wildcardValue);
    } else if (suggestionValue) {
      this.addNewFilter(type, suggestionValue);
    }
  }

  private addNewFilter(type, value) {
    this.filterAdded.emit({ detail: { type, value }});

    this.selectedType = null;
    this.keyInputVisible = true;
    this.valInputVisible = false;

    this.keyInput.nativeElement.value = '';
    this.valInput.nativeElement.value = '';
  }

  onClearClick() {
    this.filtersCleared.emit();
  }

  onRemoveFilterClick(filter) {
    this.filterRemoved.emit({ detail: filter });
  }

  onMonthSelect([month, year]) {
    this.visibleDate.setMonth(month);
    this.visibleDate.setFullYear(year);
  }

  onDaySelect(day) {
    this.visibleDate.setDate(day);
    this.dateChanged.emit({ detail: new Date(this.visibleDate) });
  }
}
