import { Component, OnInit, Input, Output, EventEmitter, HostBinding } from '@angular/core';
import { trigger, transition, style, animate, query, group, animateChild } from '@angular/animations';
import { FilterOption, FilterableOptions } from './insight-attributes-dropdown.model';
import { isEqual } from 'lodash/fp';


// Animations
const menuOpenAnim = trigger('menuOpenAnim', [
  transition(':enter', [
    style({ opacity: 0, transform: 'translateY(-8px) scale(0)' }),
    group([
      query('@childAnim', animateChild()),
      animate('220ms ease-out', style({ opacity: 1, transform: 'translateY(0px) scale(1)' }))
    ])
  ]),
  transition(':leave', [
    style({ opacity: 1 }),
    animate('195ms ease-in', style({ opacity: 0 }))
  ])
]);

const childAnim = trigger('childAnim', [
  transition(':enter', [
    style({ opacity: 0 }),
    animate('50ms 170ms', style({ opacity: 1 }))
  ])
]);

@Component({
  selector: 'app-insight-attributes-dropdown',
  templateUrl: './insight-attributes-dropdown.component.html',
  styleUrls: ['./insight-attributes-dropdown.component.scss'],
  animations: [ menuOpenAnim, childAnim ]
})
export class InsightAttributesDropdownComponent implements OnInit {

  @Input() saveAsDefault = false;
  @Input() lastSelectedOptions: string[] = []; // these are filter ids
  @Output() onUpdateFilters: EventEmitter<any> = new EventEmitter();
  @Output() onToggleMenu: EventEmitter<any> = new EventEmitter();

  @HostBinding('@menuOpenAnim') menuOpenAnim;

  public options: FilterOption[] = FilterableOptions;
  public selectedOptions: string[] = []; // these are filter ids
  public hasNewValues = false;

  ngOnInit() {
    // creating a reference to fall back on
    // will likely change after we're storing these away elsewhere
    this.lastSelectedOptions = [...this.selectedOptions];
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
    this.onUpdateFilters.emit(this.selectedOptions);
    this.onToggleMenu.emit();
  }

  public handleCancel(): void {
    this.selectedOptions.forEach(option => {
      const target = document.querySelector('.filter-button[data-filterValue="' + option + '"]');
      target.classList.remove('selected');
      target.setAttribute('aria-pressed', 'off');
    });
    this.selectedOptions = [...this.lastSelectedOptions];
    this.onToggleMenu.emit();
  }

  public handleDefaultChange(checkedState): void {
    // call to save as default
    console.log(checkedState);
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
