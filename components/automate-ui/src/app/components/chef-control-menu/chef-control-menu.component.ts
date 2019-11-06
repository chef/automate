import { Component, OnInit, HostBinding, HostListener } from '@angular/core';

@Component({
  selector: 'app-chef-control-menu',
  templateUrl: './chef-control-menu.component.html',
  styleUrls: ['./chef-control-menu.component.scss']
})
export class ChefControlMenuComponent implements OnInit {

  public isFocused = false;
  public isActive = false;
  public isDisabled = false;

  public selectedIndex = 0;
  public focusedIndex = 0;

  // can i use a ternarys here?
  @HostBinding('attr.role') comboBox = 'combobox';
  @HostBinding('attr.tabindex') get isTabable(): string { return this.isDisabled === true ? '-1' : '0'; }
  @HostBinding('class.focused') get focused(): string { return this.isFocused ? 'focused' : ''; }
  @HostBinding('class.active') get active(): string { return this.isActive ? 'active' : ''; }
  @HostBinding('class.disabled') get disabled(): string { return this.isDisabled ? 'disabled' : '';}

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

  @HostListener('keydown', ['$event']) handleKeydown(e: KeyboardEvent) {
    switch (e.key) {
      case (' '): //fallthrough
      case ('Enter'):
        return this.handleKeyActivation();
        break;
      case ('Escape'):
        console.log('Escape');
        // return this.handleEscape();
        break;
      case ('ArrowUp'):
        console.log('ArrowUp');
        // return this.handleUp(event);
        break;
      case ('ArrowDown'):
        console.log('ArrowDown');
        // return this.handleDown(event);
        break;
    }
  }

  constructor() { }

  ngOnInit() {
  }

  handleKeyActivation() {
    if (this.isDisabled) {
      return;
    } else if (this.isActive) {
      // this.makeSelection(this.focusedIndex);
      console.log('makeSelection()');
    } else {
      // this.activate(this.selectedIndex);
      console.log('activate(selectedIndex)');
    }
  }

  handleClickActivation(event) {
    if (this.isDisabled) {
      console.log('isDisabled')
      return;
    } else if (this.isActive) {
      const option = event.target.closest('chef-option');
      console.log(event.target.closest('chef-option'));
      if (!option) {
        return;
      }
      const optionId = option.optionId;
      // this.makeSelection(findIndex(['optionId', optionId], this.options));
      console.log('isActive')
    } else {
      // this.activate(this.selectedIndex);
      console.log('Not isActive')
    }
  }

}
