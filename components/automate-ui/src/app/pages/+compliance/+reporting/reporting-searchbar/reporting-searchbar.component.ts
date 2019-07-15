import {
  Component,
  EventEmitter,
  Input,
  OnInit,
  Output,
  ViewChild,
  ElementRef,
  Renderer2
} from '@angular/core';
import { clamp } from 'lodash';
import { Subject, Observable, of as observableOf } from 'rxjs';
import {
  debounceTime, switchMap, distinctUntilChanged
} from 'rxjs/operators';

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

  @ViewChild('keyInput') keyInput: ElementRef;
  @ViewChild('valInput') valInput: ElementRef;

  private suggestionsVisibleStream = new Subject<boolean>();
  private suggestionSearchTermDebounce = new Subject<any>();

  filterTypesCategories = [];
  calendarVisible = false;
  keyInputVisible = true;
  valInputVisible = false;
  filtersVisible = true;
  suggestionsVisible = false;
  isLoadingSuggestions = false;
  visibleDate;
  selectedType;
  highlightedIndex = -1;
  inputText = '';
  delayForNoSuggestions = false;

  constructor(private renderer: Renderer2) {
    // This is needed because focus is lost when clicking items.
    this.suggestionsVisibleStream.pipe(
      // wait 0.2 seconds after each lost and gain focus
      debounceTime(300),
      // switch to the latest focus change
      switchMap((active: boolean): Observable<boolean> => {
        return observableOf(active);
      })
    ).subscribe((active: boolean) => {
      this.delayForNoSuggestions = false;
      this.suggestionsVisible = active;
    });

    this.suggestionSearchTermDebounce.pipe(
      // wait 1/3 second after each keystroke before considering the term
      debounceTime(300),
      // ignore new term if same as previous term
      distinctUntilChanged()
    ).subscribe((c: any) => {
      if (c.text && c.text.length > 0) {
        this.suggestValues.emit({ detail: c});
      }
      this.isLoadingSuggestions = true;
      setTimeout(() => {
        this.isLoadingSuggestions = false;
      }, 500);
      setTimeout(() => {
        this.delayForNoSuggestions = true;
      }, 800);
    });
  }

  ngOnInit() {
    this.isLoadingSuggestions = false;
    this.visibleDate = new Date(this.date);
    this.filterTypesCategories = JSON.parse(JSON.stringify(this.filterTypes));
  }

  ngOnChange() {
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

  handleFocus(event: Event): void {
    event.stopPropagation();
    this.suggestionsVisibleStream.next(true);
  }

  handleSuggestionItemOnMouseOver(index: number): void {
    this.highlightedIndex = index;
  }

  handleInput(key, currentText): void {
    switch (key.toLowerCase()) {
      case 'arrowdown':
        this.pressArrowDown();
        break;
      case 'arrowup':
        this.pressArrowUp();
        break;
      case 'arrowleft':
        this.suggestionsVisible = true;
        break;
      case 'arrowright':
        this.suggestionsVisible = true;
        break;
      case 'enter':
        this.pressEnter(currentText);
        break;
      case 'backspace':
        this.pressBackspace(currentText);
        break;
      case 'escape':
        this.suggestionsVisible = false;
        break;
      default:
        this.pressDefaultText(currentText);
        break;
    }
  }

  pressArrowDown(): void {
    const downList =
      (this.selectedType) ? this.filterValues : this.filterTypes;
    this.highlightedIndex = clamp(
      this.highlightedIndex + 1,
      -1,
      downList.length - 1
    );
    this.suggestionsVisible = true;
  }

  pressArrowUp(): void {
    const upList =
      (this.selectedType) ? this.filterValues : this.filterTypes;
    this.highlightedIndex = clamp(
      this.highlightedIndex + -1,
      -1,
      upList.length - 1
    );
    this.suggestionsVisible = true;
  }

  pressEnter(currentText: string): void {
    if (this.selectedType) {
      this.pressEnterCategorySelected(currentText);
    } else {
      if (this.filterTypes.length > 0) {
        if (this.highlightedIndex >= 0) {
          const type = this.filterTypes[this.highlightedIndex];
          this.categorySelected(type);
        } else if (this.filterTypes.length === 1) {
          const type = this.filterTypes[0];
          this.categorySelected(type);
        }
        this.suggestionsVisible = true;
        this.showValInput();
      }
    }
  }

  pressEnterCategorySelected(currentText: string): void {
    if (this.highlightedIndex >= 0) {
      const search = this.filterValues[this.highlightedIndex];
      const type = this.selectedType;
      this.ClearAll();
      this.filterAdded.emit({
        detail: {
          value: search,
          type: type
        }
      });
    } else {
      if (currentText.indexOf('?') >= 0 || currentText.indexOf('*') >= 0) {
        const type = this.selectedType;
        this.ClearAll();
        this.filterAdded.emit({
          detail: {
            value: currentText,
            type: type
          }
        });
      } else if (this.filterValues.length > 0) {
        const foundSuggestion = this.filterValues.find((suggestion: string,
          _key: number, _iter: any) => suggestion === currentText);
        if (foundSuggestion) {
          const type = this.selectedType;
          this.ClearAll();
          this.filterAdded.emit({
            detail: {
              value: foundSuggestion,
              type: type
            }
          });
        }
      }
    }
    this.showKeyInput();
    setTimeout(() => { this.renderer.selectRootElement('#keyInput').focus(); }, 10);
  }

  pressDefaultText(currentText: string): void {
    if (this.selectedType) {
      const type = this.selectedType.name;
      this.clearSuggestions();
      this.requestForSuggestions({
        text: currentText,
        type: type
      });
      this.inputText = currentText;
    } else {
      this.updateVisibleCategories(currentText);
    }
    this.suggestionsVisible = true;
  }

  pressBackspace(currentText: string): void {
    if (this.selectedType) {
      if (this.inputText === '') {
          this.clearFilterCategories();
          this.showKeyInput();
          setTimeout(() => {
            this.keyInput.nativeElement.value = currentText;
            this.keyInput.nativeElement.focus();
          }, 10);
      } else {
        const type = this.selectedType.name;
        this.clearSuggestions();
        this.requestForSuggestions({
          text: currentText,
          type: type
        });
        this.showValInput();
        this.inputText = currentText;
      }
    } else {
      if (this.keyInput.nativeElement.value === '') {
        this.clearFilterCategories();
      } else {
        this.updateVisibleCategories(currentText);
      }
      this.showKeyInput();
      this.keyInput.nativeElement.value = currentText;
    }
  }

  updateVisibleCategories(currentText: string): void {
    this.highlightedIndex = -1;
    this.filterTypes = JSON.parse(JSON.stringify(this.filterTypesCategories));
    this.filterTypes = this.filterTypes.filter(cat => {
      return cat.title.toLowerCase().indexOf(currentText.toLowerCase()) !== -1;
    });
  }

  categorySelected(type: any): void {
    this.selectedType = type;
    this.highlightedIndex = -1;
    this.inputText = '';
    setTimeout(() => {
      this.keyInput.nativeElement.value = '';
      this.valInput.nativeElement.focus();
    }, 10);
  }

  filterClick(type, event: Event) {
    if (!this.selectedType) {
      this.categorySelected(type);
    }
    event.stopPropagation();
    this.showValInput();
    this.suggestionsVisible = false;
    setTimeout(() => this.renderer.selectRootElement('#valInput').focus(), 10);
  }

  valueClick(value: string, event: Event) {
    const type = this.selectedType;
    this.ClearAll();
    this.filterAdded.emit({
      detail: {
        type: type,
        value: value
      }
    });

    event.stopPropagation();
    this.showKeyInput();
    this.inputText = '';
    this.keyInput.nativeElement.value = '';
    setTimeout(() => { this.renderer.selectRootElement('#keyInput').focus(); }, 10);
    this.suggestionsVisibleStream.next(false);
  }

  onKeyChange(e) {
    const type = this.filterTypes.filter(t => t.title === e.target.value)[0];
    const text = '';
    if (type) {
      this.selectedType = type;
      this.suggestValues.emit({ detail: { type: type.name, text } });
      this.showValInput();
      setTimeout(() => {
        this.valInput.nativeElement.focus();
      }, 10);
    }
  }

  onValKeydown(e) {
    const text = e.target.value;
    if (e.keyCode === 8 && text.length === 0) {
      this.selectedType = undefined;
      this.showKeyInput();
      setTimeout(() => {
        this.keyInput.nativeElement.value = '';
        this.keyInput.nativeElement.focus();
      }, 10);
    }
  }

  onValInputAndFocus(e) {
    this.delayForNoSuggestions = false;
    const type = this.selectedType.name;
    const text = e.target.value;
    this.clearSuggestions();
    if (text.length > 0) {
      this.requestForSuggestions({
        text: text,
        type: type
      });
    }
    this.suggestionsVisibleStream.next(true);
  }

  onValChange(e) {
    const type = this.selectedType;
    const text: string = e.target.value;
    const suggestionValue = this.filterValues.filter(v => v.title === text)[0];
    if (this.containsWildcardChar(text)) {
      const wildcardValue = { text, title: text };
      this.addNewFilter(type, wildcardValue);
    } else if (suggestionValue && suggestionValue === undefined) {
      this.addNewFilter(type, suggestionValue);
    }
  }

  private containsWildcardChar(text: string): boolean {
    return (text.indexOf('?') >= 0 || text.indexOf('*') >= 0);
  }

  private addNewFilter(type, value) {
    this.filterAdded.emit({ detail: { type, value } });
    this.selectedType = undefined;
    this.showKeyInput();
    this.inputText = '';
    setTimeout(() => {
      this.keyInput.nativeElement.value = '';
      this.valInput.nativeElement.focus();
    }, 10);
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

  handleFocusOut() {
    this.suggestionsVisibleStream.next(false);
  }

  clearSuggestions(): void {
    this.filterValues = [];
    this.highlightedIndex = -1;
  }

  clearFilterCategories(): void {
    this.selectedType = undefined;
    this.filterTypes = JSON.parse(JSON.stringify(this.filterTypesCategories));
    this.highlightedIndex = -1;
  }

  ClearAll() {
    this.clearFilterCategories();
    this.clearSuggestions();
    this.highlightedIndex = -1;
  }

  requestForSuggestions(c: any): void {
    this.suggestionSearchTermDebounce.next(c);
  }

  // hide value input and show key input
  showKeyInput() {
    this.valInputVisible = false;
    this.keyInputVisible = true;
  }

  // hide key input and show value input
  showValInput() {
    this.keyInputVisible = false;
    this.valInputVisible = true;
  }
}
