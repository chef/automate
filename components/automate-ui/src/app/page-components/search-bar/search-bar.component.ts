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
import { clamp, compact } from 'lodash';
import { SearchBarCategoryItem, Chicklet, SuggestionItem } from '../../types/types';
import {
  debounceTime, switchMap, distinctUntilChanged
} from 'rxjs/operators';

@Component({
  selector: 'app-search-bar',
  templateUrl: './search-bar.component.html',
  styleUrls: ['./search-bar.component.scss']
})
export class SearchBarComponent implements OnChanges {
  suggestionsVisible = false;
  isLoadingSuggestions = false;
  highlightedIndex = -1;
  inputText = '';
  selectedCategoryType: SearchBarCategoryItem = undefined;
  visibleCategories = List<SearchBarCategoryItem>();
  suggestions: List<SuggestionItem> = List<SuggestionItem>();
  private suggestionsVisibleStream = new Subject<boolean>();
  private suggestionSearchTermDebounce = new Subject<Chicklet>();

  @Input() numberOfFilters: number;
  @Input() categories: SearchBarCategoryItem[] = [];
  @Input() dynamicSuggestions: string[];
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
    if (changes.dynamicSuggestions) {
      this.suggestions = List<SuggestionItem>(
        compact(changes.dynamicSuggestions.currentValue.map((suggestion) => {
        return {name: suggestion, title: suggestion};
      })));

      this.isLoadingSuggestions = false;
    }

    if (changes.categories) {
      this.visibleCategories = List<SearchBarCategoryItem>(changes.categories.currentValue);
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

  handleSuggestionClick(suggestion: SuggestionItem, event: Event): void {
    event.stopPropagation();
    const type = this.selectedCategoryType.type;
    this.clearAll();
    this.itemSelected.emit({ detail: { text: suggestion.name, type: type}});
  }

  // This is triggered when a user clicks on an item in the dropdown.
  handleCategoryItemClick(type: SearchBarCategoryItem, event: Event): void {
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
      const sug: SuggestionItem = this.suggestions.get(this.highlightedIndex);
      const type: string = this.selectedCategoryType.type;
      this.clearAll();
      this.itemSelected.emit({ detail: { text: sug.name,
        type: type }});
    } else {
      if (this.hasStaticSuggestions()) {
        if (!this.suggestions.isEmpty()) {
          if ( this.suggestions.size === 1) {
            const sug = this.suggestions.first();
            const type = this.selectedCategoryType.type;
            this.clearAll();
            this.itemSelected.emit({ detail: { text: sug.name,
              type: type}});
          }
        }
      } else {
        if (this.selectedCategoryType.allowWildcards &&
          (currentText.indexOf('?') >= 0 || currentText.indexOf('*') >= 0) ) {
          const type = this.selectedCategoryType.type;
          this.clearAll();
          this.itemSelected.emit({ detail: { text: currentText,
            type: type}});
        } else if (!this.suggestions.isEmpty()) {
          const foundSuggestion = this.suggestions.find((suggestion: SuggestionItem,
            _key: number, _iter: any) => suggestion.title === currentText);

          if (foundSuggestion) {
            const type = this.selectedCategoryType.type;
            this.clearAll();
            this.itemSelected.emit({ detail: { text: foundSuggestion.name,
              type: type}});
          }
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
        if (!this.hasStaticSuggestions()) {
          const type = this.selectedCategoryType.type;
          this.clearSuggestions();
          this.requestForSuggestions({ text: currentText,
            type: type });
        } else {
          this.updateVisibleProvidedSuggestions(currentText);
        }
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
      if (!this.hasStaticSuggestions()) {
        const type = this.selectedCategoryType.type;
        this.clearSuggestions();
        this.requestForSuggestions({ text: currentText,
          type: type });
      } else {
        this.updateVisibleProvidedSuggestions(currentText);
      }
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

  getFilterText(): string {
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
      return 'Filter by...';
    }
  }

  categorySelected(type: SearchBarCategoryItem): void {
    this.selectedCategoryType = type;
    this.inputText = '';
    this.highlightedIndex = -1;
    this.inputField.nativeElement.value = '';
    this.inputField.nativeElement.focus();
    if (this.hasStaticSuggestions()) {
      this.suggestions = List<SuggestionItem>(type.providedValues);
      this.suggestionsVisible = true;
      this.isLoadingSuggestions = false;
    }
  }

  clearCategorySelected(): void {
    this.selectedCategoryType = undefined;
    this.visibleCategories = List(this.categories);
    this.highlightedIndex = -1;
  }

  clearAll(): void {
    this.clearCategorySelected();
    this.clearSuggestions();
    this.highlightedIndex = -1;
    this.inputField.nativeElement.value = '';
  }

  clearSuggestions(): void {
    this.suggestions = List<SuggestionItem>();
    this.dynamicSuggestions = [];
    this.highlightedIndex = -1;
  }

  updateVisibleCategories(currentText: string): void {
    this.highlightedIndex = -1;
    this.visibleCategories = List(this.categories.filter(cat => {
      return cat.text.toLowerCase().indexOf(currentText.toLowerCase()) !== -1;
    }));
  }

  updateVisibleProvidedSuggestions(currentText: string): void {
    this.highlightedIndex = -1;
    this.suggestions = List(this.selectedCategoryType.providedValues.filter(
      (item: SuggestionItem) => {
      return item.title.toLowerCase().indexOf(currentText.toLowerCase()) !== -1;
    }));
  }

  requestForSuggestions(c: Chicklet): void {
    this.isLoadingSuggestions = true;
    this.suggestionSearchTermDebounce.next(c);
  }

  hasStaticSuggestions(): boolean {
    return this.selectedCategoryType.providedValues !== undefined;
  }
}
