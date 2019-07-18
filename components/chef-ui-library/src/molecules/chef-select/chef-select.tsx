import {
  Component,
  Element,
  Event,
  EventEmitter,
  Listen,
  Prop,
  State
} from '@stencil/core';
import clamp from 'lodash/fp/clamp';
import findIndex from 'lodash/fp/findIndex';
import getOr from 'lodash/fp/getOr';
import find from 'lodash/fp/find';
import lte from 'lodash/fp/lte';
import map from 'lodash/fp/map';
import max from 'lodash/fp/max';
import pipe from 'lodash/fp/pipe';

/**
 * @description
 * The `<chef-select>` molecule defines a custom select molecule. The select molecule
 * will fill the width of it's container.
 *
 * @example
 * <div style='width: 200px'>
 *   <chef-select>
 *     <chef-option>select something</chef-option>
 *     <chef-option value='opt1'>Option 1</chef-option>
 *     <chef-option value='opt2'>Option 2</chef-option>
 *     <chef-option value='opt3'>Option 3</chef-option>
 *   </chef-select>
 * </div>
 *
 * @example
 * <div style='width: 200px'>
 *   <chef-select>
 *     <chef-option>select something</chef-option>
 *     <chef-option value='opt1'>Option 1</chef-option>
 *     <chef-option value='opt2' selected>Option 2</chef-option>
 *     <chef-option value='opt3'>Option 3</chef-option>
 *   </chef-select>
 * </div>
 *
 * @example
 * <div style='width: 200px'>
 *   <chef-select value='opt3'>
 *     <chef-option>select something</chef-option>
 *     <chef-option value='opt1'>Option 1</chef-option>
 *     <chef-option value='opt2'>Option 2</chef-option>
 *     <chef-option value='opt3'>Option 3</chef-option>
 *   </chef-select>
 * </div>
 *
 * @example
 * <div style='width: 200px'>
 *   <chef-select value='opt3' change='alert("Value Changed!")'>
 *     <chef-option>select something</chef-option>
 *     <chef-option value='opt1'>Option 1</chef-option>
 *     <chef-option value='opt2'>Option 2</chef-option>
 *     <chef-option value='opt3'>Option 3</chef-option>
 *   </chef-select>
 * </div>
 *
 * @example
 * <div style='width: 200px'>
 *   <chef-select disabled='true'>
 *     <chef-option>select something</chef-option>
 *     <chef-option value='opt1'>Option 1</chef-option>
 *     <chef-option value='opt2'>Option 2</chef-option>
 *     <chef-option value='opt3'>Option 3</chef-option>
 *   </chef-select>
 * </div>
 */
@Component({
  tag: 'chef-select',
  styleUrl: 'chef-select.scss'
})
export class ChefSelect {

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

  hostData() {
    const minWidth = pipe(
      map((o: HTMLChefOptionElement) => o.getWidth()
    ), max)(this.options) || 0;

    return {
      tabindex: this.disabled ? '-1' : '0',
      role: 'combobox',
      style: { minWidth: `${minWidth + 20}px` }
    };
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
    const selected = this.options[this.selectedIndex];
    const focused = this.options[this.focusedIndex];

    this.el.setAttribute('highlighted', getOr('', 'optionId', focused));
    this.el.classList.toggle('focused', this.focused);
    this.el.classList.toggle('active', this.active);
    this.el.classList.toggle('disabled', this.disabled);

    return [
      <span class="selected-value" role="button" aria-haspopup="listbox" aria-expanded={ this.active }>
        <span class="option-content" innerHTML={ selected ? selected.getContent() : '' }></span>
        <chef-icon aria-hidden>expand_more</chef-icon>
      </span>,

      <div class="options">
        <chef-dropdown tabindex="-1" visible={ this.active } role="listbox" aria-activedescendant={ getOr('', 'optionId', focused) }>
          <slot />
        </chef-dropdown>
      </div>
    ];
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
