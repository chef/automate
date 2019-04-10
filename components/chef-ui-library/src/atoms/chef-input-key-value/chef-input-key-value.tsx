import { Component, Element, Prop, State } from '@stencil/core';

/**
 * @description
 * `chef-input-key-value` is the internal molecule used to implement the key-value
 * type for `chef-input`. For this reason it really shouldn't be used on its own.
 * Use `<chef-input type='key-value'></chef-input>` instead. See [chef-input](./chef-input)
 * for more info.
 *
 * @example
 * <chef-input type='key-value' value='foobar:bizbang'></chef-input>
 */
@Component({
  tag: 'chef-input-key-value',
  styleUrl: './chef-input-key-value.scss'
})
export class ChefInputKeyValue {

  /**
   * The value contained within the input.
   */
  @Prop({ mutable: true }) value = '';

  /**
   * The placeholder text for the input.
   */
  @Prop() placeholder = '';

  @Element() el: HTMLElement;

  @State() editing: 'key' | 'value' = 'key';

  private changeEvent = new Event('change', { bubbles: true });

  componentDidLoad() {
    this.editing = this.value ? 'value' : 'key';
  }

  render() {
    const [key, value] = this.value.split(':');

    return [
      this.editing === 'value' && key ? <chef-pill>{ key }</chef-pill> : '',
      <input
        type="text"
        value={ this.editing === 'key' ? key : value }
        placeholder={ this.placeholder }
        onKeyDown={ this.handleUpdate.bind(this, key) }
        onChange={ this.handleChange.bind(this) }/>
    ];
  }

  handleUpdate(key, event) {
    const localChangeEvent = new Event('change');

    switch (event.key) {
      case 'Tab':
        if (this.editing === 'key' && event.target.value) {
          event.preventDefault();
          event.target.dispatchEvent(localChangeEvent);
        }
        break;
      case 'Backspace':
        if (!event.target.value && this.editing === 'value') {
          this.value = key;
          this.editing = 'key';
          this.el.dispatchEvent(this.changeEvent);
        }
        break;
    }
  }

  handleChange(event) {
    event.stopPropagation();

    if (this.editing === 'key' && event.target.value) {
      this.value = event.target.value;
      this.editing = 'value';
    } else if (event.target.value) {
      this.value = `${this.value.split(':')[0]}:${event.target.value}`;
    } else {
      this.value = '';
    }

    this.el.dispatchEvent(this.changeEvent);
  }

}
