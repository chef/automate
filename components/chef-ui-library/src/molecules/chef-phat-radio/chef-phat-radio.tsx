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

  @Element() el: HTMLElement;

  @Event() change: EventEmitter;
  @Event() input: EventEmitter;

  selected: HTMLChefOptionElement;

  @Listen('click') handleClick(event) {
    const option = event.target.closest('chef-option');
    if (option) {
      this.value = option.value;
      this.change.emit();
      this.input.emit();
    }
  }

  componentDidLoad() {
    const options = this.clearOptions();
    this.selected = find(['value', this.value], options);
    if (!this.selected) {
      this.selected = this.el.querySelector('chef-option[selected]') || options[0];
      this.value = this.selected.value;
    }
    this.selected.selected = true;

    // add keypress listeners to all the child options
    const theseOptions = this.el.querySelectorAll(`chef-option`);
    theseOptions
      .forEach(option => {
        option.addEventListener('keypress', event => { this.handleKeypress(event); });
        option.setAttribute('tabindex', '0');
      });
  }

  componentDidUpdate() {
    this.clearOptions();
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

  handleKeypress = (event) => {
    if (event.key === 'Enter') {
      this.handleClick(event);
    }
  }

}
