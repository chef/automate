import { Component, Element, Event, EventEmitter, Listen, Prop, h } from '@stencil/core';
import find from 'lodash/fp/find';

/**
 * @description
 * A toggle button. Uses the chef-option atom to define it's different states.
 *
 * @example
 * <chef-phat-radio>
 *   <chef-option value='opt1'>Option 1</chef-option>
 *   <chef-option value='opt2'>Option 2</chef-option>
 *   <chef-option value='opt3'>Option 3</chef-option>
 * </chef-phat-radio>
 *
 * @example
 * <chef-phat-radio deselectable value="opt2">
 *   <chef-option value='opt1'>Option 1</chef-option>
 *   <chef-option value='opt2'>Option 2</chef-option>
 *   <chef-option value='opt3'>Option 3</chef-option>
 * </chef-phat-radio>
 */
@Component({
  tag: 'chef-phat-radio',
  styleUrl: './chef-phat-radio.scss'
})
export class ChefPhatRadio {

  /**
   * The value of the currently toggled option.
   */
  @Prop({ mutable: true }) value = '';

  /**
   * An optional property that when true allows users to deselect an option by selecting a currently selected option
   */
  @Prop() deselectable = false;

  @Element() el: HTMLElement;

  @Event() change: EventEmitter;
  @Event() input: EventEmitter;

  selected: HTMLChefOptionElement;

  @Listen('click') handleClick(event: MouseEvent) {
    const target = event.target as HTMLElement;
    const option = target.closest('chef-option');
    if (option) {
      this.selectOption(option);
    }
  }

  @Listen('keypress') handleKeypress(event: KeyboardEvent) {
    const target = event.target as HTMLElement;
    const option = target.closest('chef-option');
    if (option && event.key === 'Enter') {
      this.selectOption(option);
    }
  }

  componentDidLoad() {
    const options = this.clearOptions();
    this.selected = find(['value', this.value], options);
    // When option to deselect is true, we also do not want to make a default
    // selection upon load, so we return early unless a value has been explicity set by prop
    if (!this.selected && this.deselectable) { return; }

    if (!this.selected) {
      this.selected = this.el.querySelector('chef-option[selected]') || options[0];
      this.value = this.selected.value;
    }
    this.selected.selected = true;

    // Set tabindex on options to make them keyboard focusable
    options.forEach(option => {
      option.setAttribute('tabindex', '0');
    });
  }

  componentDidUpdate() {
    this.clearOptions();
    // Return early if the update is empty, because this means we are deselecting
    if (this.deselectable && this.value === '') { return; }

    this.selected = this.el.querySelector(`chef-option[value='${this.value}']`);
    this.selected.selected = true;
  }

  render() {
    return (
      <slot />
    );
  }

  clearOptions(): HTMLChefOptionElement[] {
    const options: HTMLChefOptionElement[] = Array.from(this.el.querySelectorAll('chef-option'));
    options.forEach((opt) => opt.selected = false);
    return options;
  }

  selectOption(option: HTMLChefOptionElement) {
    const isSameOption = this.value === option.value;
    this.value = this.deselectable && isSameOption ? '' : option.value;
    this.change.emit();
    this.input.emit();
  }

}
