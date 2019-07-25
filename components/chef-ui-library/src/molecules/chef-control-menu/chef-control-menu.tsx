import { Component, Element, Event, EventEmitter, Host, Listen, Prop, State, h } from '@stencil/core';
import clamp from 'lodash/fp/clamp';
import findIndex from 'lodash/fp/findIndex';
import getOr from 'lodash/fp/getOr';
import find from 'lodash/fp/find';
import lte from 'lodash/fp/lte';

/**
 * @description
 * The `<chef-control-menu>` molecule defines a custom select molecule.
 *
 * @example
 * <div style="text-align:center;">
 *   <chef-control-menu>
 *     <chef-option value='opt1'>Delete User</chef-option>
 *   </chef-control-menu>
 * </div>
 *
 * @example
 * <div style="text-align:center;">
 *   <chef-control-menu>
 *     <chef-option value='opt1'>Copy Token ID</chef-option>
 *     <chef-option value='opt2'>Toggle Status</chef-option>
 *     <chef-option value='opt3'>Delete Token</chef-option>
 *     <chef-option value='opt3'>This is a very long option</chef-option>
 *   </chef-control-menu>
 * </div>
 *
 * @example
 * <div style="text-align:center;">
 *   <chef-control-menu disabled='true'>
 *     <chef-option value='opt1'>Option 1</chef-option>
 *     <chef-option value='opt2'>Option 2</chef-option>
 *     <chef-option value='opt3'>Option 3</chef-option>
 *   </chef-control-menu>
 * </div>
 *
 * @example
 * <chef-table id="control-menu-example-table">
 *   <chef-tr>
 *     <chef-th>ID</chef-th>
 *     <chef-th>Name</chef-th>
 *     <chef-th>Platform</chef-th>
 *     <chef-th>Status</chef-th>
 *     <chef-th style="max-width:50px;"></chef-th>
 *   </chef-tr>
 *   <chef-tr>
 *     <chef-td>1</chef-td>
 *     <chef-td>node-1</chef-td>
 *     <chef-td>platform-1</chef-td>
 *     <chef-td>passed</chef-td>
 *     <chef-td style="max-width:50px;">
 *      <chef-control-menu>
 *        <chef-option value='opt1'>Copy Token ID</chef-option>
 *        <chef-option value='opt2'>Toggle Status</chef-option>
 *        <chef-option value='opt3'>Delete Token</chef-option>
 *        <chef-option value='opt3'>This is a very long option</chef-option>
 *      </chef-control-menu>
 *     </chef-td>
 *   </chef-tr>
 * </chef-table>
 */
@Component({
  tag: 'chef-control-menu',
  styleUrl: 'chef-control-menu.scss'
})
export class ChefControlMenu {

  /**
   * The selected value
   */
  @Prop({ mutable: true }) value: string;

  /**
   * Disable your select menu by setting this to true
   */
  @Prop() disabled = false;

  @State() options: HTMLChefOptionElement[] = [];
  @State() selectedIndex = 0;
  @State() focusedIndex = 0;
  @State() focused = false;
  @State() active = false;

  /**
   * Emitted when the value of the molecule changes.
   */
  @Event() change: EventEmitter;

  @Element() el: HTMLElement;

  @Listen('focus') handleFocus() {
    if (!this.disabled) {
      this.focused = true;
    }
  }

  @Listen('focusout') handleFocusOut(event) {
    const relatedTarget = event.relatedTarget;
    event.stopPropagation();
    if (!relatedTarget || relatedTarget.nodeName !== 'CHEF-DROPDOWN') {
      this.focused = false;
      this.active = false;
    }
  }

  @Listen('click') handleClickActivation(event) {
    if (this.disabled) {
      return;
    } else if (this.active) {
      const option = event.target.closest('chef-option');
      if (!option) return;
      const optionId = option.optionId;
      this.makeSelection(findIndex(['optionId', optionId], this.options));
    } else {
      this.activate(this.selectedIndex);
    }
  }

  @Listen('keydown') handleKeydown(event: KeyboardEvent) {
    switch (event.key) {
      case (' '):
      case ('Enter'):
        return this.handleKeyActivation();
      case ('Escape'):
        return this.handleEscape();
      case ('ArrowUp'):
        return this.handleUp(event);
      case ('ArrowDown'):
        return this.handleDown(event);
    }
  }

  handleKeyActivation() {
    if (this.disabled) {
      return;
    } else if (this.active) {
      this.makeSelection(this.focusedIndex);
    } else {
      this.activate(this.selectedIndex);
    }
  }

  handleEscape() {
    this.active = false;
    this.focusedIndex = this.selectedIndex;
  }

  handleUp(event: KeyboardEvent) {
    event.preventDefault();
    this.focusedIndex = this.clamp(this.focusedIndex - 1);
  }

  handleDown(event: KeyboardEvent) {
    event.preventDefault();
    this.focusedIndex = this.clamp(this.focusedIndex + 1);
  }

  componentDidLoad() {
    this.options = Array.from(this.el.querySelectorAll('chef-option'));
    // The default option is determined by first checking the value property, then
    // looking for a selected attribute on the options list and finally defaulting
    // to the first option if all else fails.
    const defaultIndexes = [ findIndex(['value', this.value], this.options),
                             findIndex('selected', this.options),
                             0 ];

    const index = this.clamp(find(lte(0), defaultIndexes));

    this.value = getOr('', [index, 'value'], this.options);
    this.selectedIndex = index;
    this.focusedIndex = index;
    this.syncOptions(index);
  }

  render() {
    const focused = this.options[this.focusedIndex];
    const classNames = [
      this.focused ? 'focused' : '',
      this.active ? 'active' : '',
      this.disabled ? 'disabled' : ''
    ].join(' ');

    return (
      <Host tabindex={this.disabled ? '-1' : '0'} role="combobox" class={classNames}>
        <span
          class="control-menu"
          role="button"
          aria-haspopup="listbox"
          aria-expanded={ this.active }>
          <span class="option-content"></span>
          <chef-icon aria-hidden>more_horiz</chef-icon>
        </span>
        <div class="options">
          <chef-dropdown
            tabindex="-1"
            visible={ this.active }
            role="listbox"
            aria-activedescendant={ getOr('', 'optionId', focused) }>
            <slot />
          </chef-dropdown>
        </div>
      </Host>
    );
  }

  private clamp(value: number) {
    return clamp(0, this.options.length - 1, value);
  }

  private makeSelection(index) {
    const changeEvent = new CustomEvent('change');

    this.selectedIndex = index;
    this.active = false;
    this.value = getOr('', [index, 'value'], this.options);
    this.change.emit(changeEvent);
    this.syncOptions(index);
  }

  private activate(index) {
    const dropdown: HTMLElement = this.el.querySelector('chef-dropdown');
    this.active = true;
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
