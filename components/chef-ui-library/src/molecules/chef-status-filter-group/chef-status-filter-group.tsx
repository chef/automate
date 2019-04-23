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
 * A toggle button. Uses the chef-option atom to define it's different states.
 *
 * @example
 * <chef-status-filter-group>
 *   <chef-option class="filter general" value="general">
 *     <div class="filter-label">Total</div>
 *     <chef-icon>group_work</chef-icon><div class="filter-total">30</div>
 *   </chef-option>
 *   <chef-option class="filter critical" value='critical'>
 *     <div class="filter-label">Critical</div>
 *     <chef-icon>warning</chef-icon><div class="filter-total">10</div>
 *   </chef-option>
 *   <chef-option class="filter warning" value='warning'>
 *     <div class="filter-label">Warning</div>
 *     <chef-icon>error</chef-icon><div class="filter-total">5</div>
 * </chef-option>
 *   <chef-option class="filter success" value='success'>
 *     <div class="filter-label">OK</div>
 *     <chef-icon>check_circle</chef-icon><div class="filter-total">5</div>
 * </chef-option>
 *   <chef-option class="filter unknown" value='unknown'>
 *     <div class="filter-label">Deploying</div>
 *     <chef-icon>help</chef-icon><div class="filter-total">10</div>
 *   </chef-option>
 * </chef-status-filter-group>
 */
@Component({
  tag: 'chef-status-filter-group',
  styleUrl: './chef-status-filter-group.scss'
})
export class ChefStatusFilterGroup {

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
