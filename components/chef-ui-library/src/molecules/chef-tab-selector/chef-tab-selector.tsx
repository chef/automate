import { Component,
  Element,
  Event,
  EventEmitter,
  Listen,
  Prop
} from '@stencil/core';
import find from 'lodash/fp/find';

/**
 * @description
 * A selection of tabs. Uses the chef-option atom to define its different states.
 *
 * @example
 * <chef-tab-selector>
 *   <chef-option value='opt1'>Option 1</chef-option>
 *   <chef-option value='opt2'>Option 2</chef-option>
 *   <chef-option value='opt3'>Option 3</chef-option>
 * </chef-tab-selector>
 *
 * @example
 * <chef-tab-selector value='opt2'>
 *   <chef-option value='opt1'>Option 1</chef-option>
 *   <chef-option value='opt2'>Option 2</chef-option>
 *   <chef-option value='opt3'>Option 3</chef-option>
 * </chef-tab-selector>
 *
 * @example
 * <chef-tab-selector>
 *   <chef-option value='opt1'><chef-icon>thumb_up</chef-icon></chef-option>
 *   <chef-option value='opt2'><chef-icon>thumb_down</chef-icon></chef-option>
 * </chef-tab-selector>
 *
 * @example
 * <script>
 *   const toggle = document.querySelector('#ex1toggle');
 *   toggle.addEventListener('change', (event) => alert(event.target.value));
 * </script>
 *
 * <chef-tab-selector id='ex1toggle'>
 *   <chef-option value='opt1'>Option 1</chef-option>
 *   <chef-option value='opt2'>Option 2</chef-option>
 *   <chef-option value='opt3'>Option 3</chef-option>
 * </chef-tab-selector>
 */
@Component({
  tag: 'chef-tab-selector',
  styleUrl: './chef-tab-selector.scss'
})
export class ChefTabSelector {

  /**
   * The value of the currently selected tab.
   */
  @Prop({ mutable: true }) value = '';

  @Element() el: HTMLElement;

  @Event() change: EventEmitter;
  @Event() input: EventEmitter;

  selected: HTMLChefOptionElement;

  @Listen('click') handleClick(event) {
    this.value = event.target.closest('chef-option').value;
    this.change.emit();
    this.input.emit();
  }

  componentDidLoad() {
    const options = this.clearOptions();
    this.selected = find(['value', this.value], options);
    if (!this.selected) {
      this.selected = this.el.querySelector('chef-option[selected]') || options[0];
      this.value = this.selected.value;
    }
    this.selected.selected = true;
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

}
