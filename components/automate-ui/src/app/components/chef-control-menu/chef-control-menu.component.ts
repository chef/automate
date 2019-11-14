import { Component, OnInit, HostBinding, HostListener, ElementRef } from '@angular/core';
import { clamp, findIndex, getOr, find, lte } from 'lodash/fp';

import { HTMLChefOptionElement } from 'app/types/types';


@Component({
  selector: 'chef-control-menu-new',
  templateUrl: './chef-control-menu.component.html',
  styleUrls: ['./chef-control-menu.component.scss']
})
export class ChefControlMenuComponent implements OnInit {

  public isFocused = false;
  public isActive = false;
  public isDisabled = false;

  public options: HTMLChefOptionElement[] = [];
  public selectedIndex = 0;
  public focusedIndex = 0;
  public value: any; // <-- needs to be a type but string and number don't work

  // can i use a ternarys here?
  @HostBinding('attr.role') role = 'combobox';
  @HostBinding('attr.tabindex') get isTabable(): string { return this.isDisabled === true ? '-1' : '0'; }
  @HostBinding('class.focused') get focused(): boolean { return this.isFocused; }
  @HostBinding('class.active') get active(): boolean { return this.isActive; }
  @HostBinding('class.disabled') get disabled(): boolean { return this.isDisabled; }

  @HostListener('focus') handleFocus() {
    if (!this.isDisabled) {
      this.isFocused = true;
    }
  }

  @HostListener('focusout', ['$event']) handleFocusOut(e) {
    const relatedTarget = e.relatedTarget;
    e.stopPropagation();
    if (!relatedTarget || relatedTarget.nodeName !== 'CHEF-DROPDOWN') {
      this.isFocused = false;
      this.isActive = false;
    }
  }

  @HostListener('click', ['$event']) handleClickActivation(event: MouseEvent) {
    if (this.isDisabled) {
      return;
    } else if (this.isActive) {
      console.log(event)
      const option = event.target.closest('chef-option-new');
      if (!option || event.target.nodeName === 'CHEF-ICON') {
        this.isActive = false;
        return;
      }
      const optionId = option.optionId;
      this.makeSelection(findIndex(['optionId', optionId], this.options));
    } else {
      this.activate(this.selectedIndex);
    }
  }

  @HostListener('keydown', ['$event']) handleKeydown(e: KeyboardEvent) {
    switch (e.key) {
      case (' '): // fallthrough
      case ('Enter'):
        return this.handleKeyActivation();
        break;
      case ('Escape'):
        return this.handleEscape();
        break;
      case ('ArrowUp'):
        return this.handleUp(e);
        break;
      case ('ArrowDown'):
        return this.handleDown(e);
        break;
    }
  }

  constructor( private el: ElementRef) { }

  ngOnInit() {
    this.options = Array.from(this.el.nativeElement.querySelectorAll('chef-option-new'));
    // The default option is determined by first checking the value property, then
    // looking for a selected attribute on the options list and finally defaulting
    // to the first option if all else fails.
    const defaultIndexes = [findIndex(['value', this.value], this.options),
    findIndex('selected', this.options),
      0];

    const index = this.clamp(find(lte(0), defaultIndexes));

    this.value = getOr('', [index, 'value'], this.options);
    this.selectedIndex = index;
    this.focusedIndex = index;
    this.syncOptions(index);
  }

  handleKeyActivation() {
    if (this.isDisabled) {
      return;
    } else if (this.isActive) {
      this.makeSelection(this.focusedIndex);
    } else {
      this.activate(this.selectedIndex);
    }
  }

  handleEscape() {
    this.isActive = false;
    this.focusedIndex = this.selectedIndex;
  }

  handleUp(e: KeyboardEvent) {
    e.preventDefault();
    this.focusedIndex = this.clamp(this.focusedIndex - 1);
  }

  handleDown(e: KeyboardEvent) {
    e.preventDefault();
    this.focusedIndex = this.clamp(this.focusedIndex + 1);
  }
  private clamp(value: number) {
    return clamp(0, this.options.length - 1, value);
  }

  private makeSelection(index) {
    // const changeEvent = new CustomEvent('change');

    this.selectedIndex = index;
    this.isActive = false;
    this.value = getOr('', [index, 'value'], this.options);
    // this.change.emit(changeEvent);
    this.syncOptions(index);
  }

  private activate(index) {
    // check that this is an OK use of nativeElement, depending on our use case
    // it may not work with webworkers https://alligator.io/angular/using-renderer2/
    const dropdown: HTMLElement = this.el.nativeElement.querySelector('chef-dropdown');
    this.isActive = true;
    this.focusedIndex = this.clamp(index);
    dropdown.focus();
  }

  private syncOptions(index) {
    if (this.options[index]) {
      this.options.forEach((opt) => opt.selected = false);
      this.options[index].selected = true;
    }
  }


}
