import {
  Component,
  EventEmitter,
  Input,
  Output,
  OnChanges,
  SimpleChanges,
  ViewChild,
  ElementRef,
  Renderer2
} from '@angular/core';
import { Subject, Observable, of as observableOf } from 'rxjs';
import { List } from 'immutable';
import { clamp } from 'lodash';
import { Chicklet } from '../../types/types';
import {
  debounceTime, switchMap, distinctUntilChanged
} from 'rxjs/operators';

@Component({
  selector: 'app-client-runs-search-bar',
  templateUrl: './client-runs-search-bar.component.html',
  styleUrls: ['./client-runs-search-bar.component.scss']
})
export class ClientRunsSearchBarComponent implements OnChanges {
  suggestionsVisible = false;
  isLoadingSuggestions = false;
  highlightedIndex = -1;
  inputText = '';
  selectedCategoryType: Chicklet = undefined;
  visibleCategories = List<Chicklet>();
  suggestions: List<string> = List<string>();
  private suggestionsVisibleStream = new Subject<boolean>();
  private suggestionSearchTermDebounce = new Subject<Chicklet>();

  @Input() numberOfFilters: number;
  @Input() filterTypes: Chicklet[] = [];
  @Input() filterValues: string[];
  @Output() suggestValues: EventEmitter<any> = new EventEmitter<any>();
  @Output() itemSelected: EventEmitter<any> = new EventEmitter<any>();
  @Output() filtersButtonClick: EventEmitter<any> = new EventEmitter<any>();

  @ViewChild('search_box') inputField: ElementRef;

  constructor(private renderer: Renderer2) {
    // This is needed because focus is lost when clicking items.
    this.suggestionsVisibleStream.pipe(
      // wait 0.2 seconds after each lost and gain focus
      debounceTime(200),
      // switch to the latest focus change
      switchMap((active: boolean): Observable<boolean> => {
        return observableOf(active);
      })
    ).subscribe((active: boolean) => {
      this.suggestionsVisible = active;
    });

    this.suggestionSearchTermDebounce.pipe(
      // wait 1/3 second after each keystroke before considering the term
      debounceTime(300),
      // ignore new term if same as previous term
      distinctUntilChanged()
    ).subscribe((c: Chicklet) => {
      if (c.text && c.text.length > 0) {
        this.isLoadingSuggestions = true;
        this.suggestValues.emit({ detail: c });
      }
    });
  }

  ngOnChanges(changes: SimpleChanges) {
    if (changes.filterValues) {
      if (changes.filterValues.currentValue.length > 0 && changes.filterValues.currentValue[0] !== undefined){
        this.suggestions = List<string>(changes.filterValues.currentValue);
      } else {
        this.suggestions = List<string>();
      }
      this.isLoadingSuggestions = false;
    }

    if (changes.filterTypes) {
      this.visibleCategories = List<Chicklet>(changes.filterTypes.currentValue);
    }
  }

  handleFiltersClick() {
    this.filtersButtonClick.emit();
  }

  handleFocusOut() {
    this.suggestionsVisibleStream.next(false);
  }

  handleCategoryClick(): void {
    this.clearAll();
    this.suggestionsVisibleStream.next(true);
    this.renderer.selectRootElement('#search_box').focus();
  }

  // Handlers
  // We bind to both the focus and click events here. We do that because the dropdown
  // is closed via the app_click action that is triggered from the app component. We
  // need to stopPropagation here to prevent the app_click action from triggering and
  // immediately closing the dropdown.
  handleFocus(event: Event): void {
    event.stopPropagation();
    this.suggestionsVisibleStream.next(true);
  }

  handleSuggestionClick(suggestion: string, event: Event): void {
    event.stopPropagation();
    const type = this.selectedCategoryType.type;
    this.clearAll();
    this.itemSelected.emit({ detail: { text: suggestion,
      type: type}});
  }

  // This is triggered when a user clicks on an item in the dropdown.
  handleCategoryItemClick(type: Chicklet, event: Event): void {
    event.stopPropagation();
    if (!this.selectedCategoryType) {
      this.categorySelected(type);
    }
  }

  handleSuggestionItemOnMouseOver(index: number): void {
    this.highlightedIndex = index;
  }

  handleCategoryItemOnMouseOver(index: number): void {
    this.highlightedIndex = index;
  }

  pressArrowDown(): void {
    const downList =
    (this.selectedCategoryType) ? this.suggestions : this.visibleCategories;
    this.highlightedIndex = clamp(
      this.highlightedIndex + 1,
      -1,
      downList.size - 1
    );
    this.suggestionsVisible = true;
  }

  pressArrowUp(): void {
    const upList =
    (this.selectedCategoryType) ? this.suggestions : this.visibleCategories;
    this.highlightedIndex = clamp(
      this.highlightedIndex + -1,
      -1,
      upList.size - 1
    );
    this.suggestionsVisible = true;
  }

  pressEnterCategorySelected(currentText: string): void {
    if (this.highlightedIndex >= 0) {
      const search = this.suggestions.get(this.highlightedIndex);
      const type = this.selectedCategoryType.type;
      this.clearAll();
      this.itemSelected.emit({ detail: { text: search,
        type: type }});
    } else {
      if (currentText.indexOf('?') >= 0 || currentText.indexOf('*') >= 0) {
        const type = this.selectedCategoryType.type;
        this.clearAll();
        this.itemSelected.emit({ detail: { text: currentText,
          type: type}});
      } else if (!this.suggestions.isEmpty()) {
        const foundSuggestion = this.suggestions.find((suggestion: string,
          _key: number, _iter: any) => suggestion === currentText);

        if (foundSuggestion) {
          const type = this.selectedCategoryType.type;
          this.clearAll();
          this.itemSelected.emit({ detail: { text: foundSuggestion,
            type: type}});
        }
      }
    }
  }

  pressEnter(currentText: string): void {
    if (this.selectedCategoryType) {
      this.pressEnterCategorySelected(currentText);
    } else {
      if (!this.visibleCategories.isEmpty()) {
        if (this.highlightedIndex >= 0) {
          const type = this.visibleCategories.get(this.highlightedIndex);
          this.categorySelected(type);
        } else if ( this.visibleCategories.size === 1) {
          const type = this.visibleCategories.first();
          this.categorySelected(type);
        }
      }
    }
    this.suggestionsVisible = true;
  }

  pressBackspace(currentText: string): void {
    if (this.selectedCategoryType) {
      if (this.inputText === '') {
        this.clearCategorySelected();
      } else {
        const type = this.selectedCategoryType.type;
        this.clearSuggestions();
        this.requestForSuggestions({ text: currentText,
          type: type });
      }
    } else {
      if (this.inputText === '') {
        this.clearCategorySelected();
      } else {
        this.updateVisibleCategories(currentText);
      }
    }
    this.inputText = currentText;
    this.suggestionsVisible = true;
  }

  pressDefaultText(currentText: string): void {
    if (this.selectedCategoryType) {
      const type = this.selectedCategoryType.type;
      this.clearSuggestions();
      this.requestForSuggestions({ text: currentText,
        type: type });
    } else {
      this.updateVisibleCategories(currentText);
    }
    this.inputText = currentText;
    this.suggestionsVisible = true;
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

  get_filter_text(): string {
    if (this.selectedCategoryType) {
      switch (this.selectedCategoryType.type) {
        case 'platform':
          return 'Enter a platform type';
        case 'policy_revision':
          return 'Enter a revision ID number';
        case '':
          return 'Filter nodes by …';
        default:
          const normal_type = this.selectedCategoryType.text.replace(
            '_name', '').split('_').join(' ');
          return `Enter ${normal_type} name`;
      }
    } else {
      return 'Enter name';
    }
  }

  categorySelected(type: Chicklet): void {
    this.selectedCategoryType = type;
    this.inputText = '';
    this.highlightedIndex = -1;
    this.inputField.nativeElement.value = '';
    this.inputField.nativeElement.focus();
  }

  clearCategorySelected(): void {
    this.selectedCategoryType = undefined;
    this.visibleCategories = List(this.filterTypes);
    this.highlightedIndex = -1;
  }

  clearAll(): void {
    this.clearCategorySelected();
    this.clearSuggestions();
    this.highlightedIndex = -1;
    this.inputField.nativeElement.value = '';
  }

  clearSuggestions(): void {
    this.suggestions = List<string>();
    this.filterValues = [];
    this.highlightedIndex = -1;
  }

  updateVisibleCategories(currentText: string): void {
    this.highlightedIndex = -1;
    this.visibleCategories = List(this.filterTypes.filter(cat => {
      return cat.text.toLowerCase().indexOf(currentText.toLowerCase()) !== -1;
    }));
  }

  requestForSuggestions(c: Chicklet): void {
    this.isLoadingSuggestions = true;
    this.suggestionSearchTermDebounce.next(c);
  }
}
