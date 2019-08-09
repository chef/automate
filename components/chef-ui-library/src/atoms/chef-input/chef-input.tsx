import { Component, Host, Listen, Prop, State, h } from '@stencil/core';

/**
 * @description
 * An atom for defining inputs with chef specific styling. The input type
 * is supplied via the type prop and in most cases just passed down to the
 * internal input. So any type valid on a standard input should be valid
 * for chef-input. There are also some additional custom types that can
 * provide some added functionality. Currently the only additional type
 * provided is `key-value`. The key/value can be provided via the value
 * attribute as a ':' delimited string, 'key:value'.
 *
 * The underlying <input> element has autocomplete set to "off".
 *
 * @example
 * <chef-input placeholder='Enter value...'></chef-input>
 *
 * @example
 * <chef-input disabled placeholder='Cannot change value'></chef-input>
 *
 * @example
 * <chef-input value='foobar' placeholder='Enter value...'></chef-input>
 *
 * @example
 * <chef-input type='key-value' placeholder='Enter value...'></chef-input>
 *
 * @example
 * <chef-input type='key-value' value='foobar:bizbang' placeholder='Enter value...'></chef-input>
 */
@Component({
  tag: 'chef-input',
  styleUrl: './chef-input.scss'
})
export class ChefInput {

  /**
   * The value contained within the input.
   */
  @Prop({ mutable: true }) value = '';

  /**
   * The type of input to use
   */
  @Prop() type = 'text';

  /**
   * The placeholder text for the input.
   */
  @Prop() placeholder = '';

  /**
   * Indicate input as disabled
   */
  @Prop({ reflectToAttr: true }) disabled = false;

  @State() focused = false;

  @Listen('focusin') handleFocusin() {
    this.focused = true;
  }

  @Listen('focusout') handleFocusout() {
    this.focused = false;
  }

  render() {
    return (
      <Host class={this.focused ? 'focused' : ''}>
        {this.renderContent()}
      </Host>
    );
  }

  renderContent() {
    switch (this.type) {
      case 'key-value':
        return this.renderKeyValue();
      default:
        return this.renderDefault();
    }
  }

  renderKeyValue() {
    return (
      <chef-input-key-value
        value={ this.value }
        placeholder={ this.placeholder }
        onChange={ this.handleChange.bind(this)
        }></chef-input-key-value>
    );
  }

  renderDefault() {
    return (
      <input type={ this.type }
        value={ this.value }
        placeholder={ this.placeholder }
        onChange={ this.handleChange.bind(this) }
        disabled={this.disabled}
        aria-disabled={this.disabled}
        autocomplete="off"/>
    );
  }

  handleChange(event) {
    this.value = event.target.value;
  }

}
