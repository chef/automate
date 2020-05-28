import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { FilterOption, FilterableOptions } from './insight-attributes-dropdown.model';
import { isEqual } from 'lodash/fp';

@Component({
  selector: 'app-insight-attributes-dropdown',
  templateUrl: './insight-attributes-dropdown.component.html',
  styleUrls: ['./insight-attributes-dropdown.component.scss']
})
export class InsightAttributesDropdownComponent implements OnInit {

  @Input() saveAsDefault = true;
  @Input() lastSelectedOptions: string[] = []; // these are filter ids
  @Output() onUpdateFilters: EventEmitter<any> = new EventEmitter();
  @Output() onToggleMenu: EventEmitter<any> = new EventEmitter();

  public options: FilterOption[] = FilterableOptions;
  public selectedOptions: string[] = []; // these are filter ids
  private initialDefaultSavePref = false;
  public hasNewValues = false;
  public saveDefaultChanged = false;

  ngOnInit() {
    // creating a reference to fall back on
    // will likely change after we're storing these away elsewhere
    this.lastSelectedOptions = [...this.selectedOptions];
    this.initialDefaultSavePref = this.saveAsDefault;
  }

  get disableSubmit() {
    return this.saveDefaultChanged || this.hasNewValues ? false : true;
  }

  public handleSelect(event: Event) {
    const target = event.target as HTMLElement;
    const filter = target.getAttribute('data-filterValue');

    const isFilteredIndex = this.selectedOptions.indexOf(filter);
    if ( isFilteredIndex >= 0 ) {
      this.selectedOptions.splice(isFilteredIndex, 1);
      this.updateButtonState(target, 'off');
    } else {
      if (this.selectedOptions.length < 5) {
        this.selectedOptions.push(filter);
        this.updateButtonState(target, 'on');
      }
    }

    // check if form should be valid based on new selected options
    this.hasNewValues = !isEqual(this.lastSelectedOptions.sort(), this.selectedOptions.sort());
  }

  public handleUpdate(): void {
    // emit new selected filter ids
    this.onUpdateFilters.emit({
      filters: this.selectedOptions,
      saveAsDefault: this.saveAsDefault
    });
    this.onToggleMenu.emit();
  }

  public handleCancel(): void {
    this.selectedOptions.forEach(option => {
      const target = document.querySelector(`.filter-button[data-filterValue="${option}"]`);
      target.classList.remove('selected');
      target.setAttribute('aria-pressed', 'off');
    });
    this.selectedOptions = [...this.lastSelectedOptions];
    this.onToggleMenu.emit();
  }

  public handleSaveAsDefaultChange(): void {
    this.saveAsDefault = !this.saveAsDefault;
    this.saveDefaultChanged = this.saveAsDefault !== this.initialDefaultSavePref ? true : false;
  }

  private updateButtonState(target: HTMLElement, state: 'on' | 'off'): void {
    if ( state === 'off') {
      target.classList.remove('selected');
      target.setAttribute('aria-pressed', 'false');
    } else {
      target.classList.add('selected');
      target.setAttribute('aria-pressed', 'true');
    }
  }

}
