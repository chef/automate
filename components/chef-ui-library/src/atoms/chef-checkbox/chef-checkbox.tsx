import {
  Component,
  Event,
  EventEmitter,
  Listen,
  Prop,
  State
} from '@stencil/core';

let id = 0;

/**
 * @description
 * `<chef-checkbox>` is used to display checkbox inputs. Clicking the checkbox,
 * or pressing `space` while the checkbox is focused, will toggle the `checked`
 * property. A `change` event emits when the `checked` property is toggled.
 *
 * @example
 * <div class="form-field">
 *   <chef-checkbox>Label Text</chef-checkbox>
 * </div>
 * <div class="form-field">
 *   <chef-checkbox checked>Label Text</chef-checkbox>
 * </div>
 * <div class="form-field">
 *   <chef-checkbox disabled>Label Text</chef-checkbox>
 * </div>
 * <div class="form-field">
 *   <chef-checkbox indeterminate>Label Text</chef-checkbox>
 * </div>
 */
@Component({
  tag: 'chef-checkbox',
  styleUrl: './chef-checkbox.scss'
})
export class ChefCheckbox {

  @Prop({ mutable: true }) checked = false;

  @Prop({ mutable: true }) indeterminate = false;

  @Prop() disabled = false;

  @Event() change: EventEmitter;

  @Listen('keydown') handleKeydown(event: KeyboardEvent) {
    if (event.key === ' ') {
      this.handleToggle(event);
    }
  }

  @Listen('click') handleToggle(event) {
    if (event.target.nodeName !== 'A') {
      this.toggle();
      event.preventDefault();
    }
  }

  @State() labelId = '';

  @State() labelled = false;

  componentWillLoad() {
    this.labelId = `label-${++id}`;
  }

  toggle() {
    this.checked = !this.checked;
    this.indeterminate = false;
    this.change.emit(this.checked);
  }

  hostData() {
    return {
      'role': 'checkbox',
      'tabindex': this.disabled ? '-1' : '0',
      'aria-checked': this.checked ? 'true' : this.indeterminate ? 'mixed' : 'false',
      'aria-disabled': this.disabled ? 'true' : null,
      'aria-labelledby': this.labelled ? this.labelId : null
    };
  }

  render() {
    const checkIcon = this.checked ? 'check' : this.indeterminate ? 'remove' : 'close';
    const labelRef = el => this.labelled = el && el.innerHTML.trim().length > 0;
    return ([
      <div class="check-wrap">
        <chef-icon>{checkIcon}</chef-icon>
      </div>,
      <div class="label-wrap" id={this.labelId} ref={labelRef}>
        <slot />
      </div>
    ]);
  }
}
