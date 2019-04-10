import { Component, Element, Method, Prop } from '@stencil/core';

let id = 0;

export interface ChefOption extends HTMLElement {
  value: string;
  (getId): string;
  (getContents): string;
  (getValue): string;
}

/**
 * @description
 * The `<chef-option>` atom is used to specify options for the [`<chef-select>`](/molecules/chef-select),
 * [`<chef-toggle>`](/molecules/chef-toggle), [`<chef-radio>`](/molecules/chef-radio), and
 * [`<chef-phat-radio>`](/molecules/chef-phat-radio) molecules.
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
 *     <chef-option value='opt1' icon='assignment_ind'>Option 1</chef-option>
 *     <chef-option value='opt2' icon='chrome_reader_mode'>Option 2</chef-option>
 *     <chef-option value='opt3' icon='business_center'>Option 3</chef-option>
 *   </chef-select>
 * </div>
 *
 * @example
 * <chef-toggle>
 *   <chef-option value='opt1'>Option 1</chef-option>
 *   <chef-option value='opt2'>Option 2</chef-option>
 *   <chef-option value='opt3'>Option 3</chef-option>
 * </chef-toggle>
 *
 * @example
 * <chef-toggle value='opt2'>
 *   <chef-option value='opt1'>Option 1</chef-option>
 *   <chef-option value='opt2'>Option 2</chef-option>
 *   <chef-option value='opt3'>Option 3</chef-option>
 * </chef-toggle>
 *
 * @example
 * <chef-toggle>
 *   <chef-option value='opt1'><chef-icon>thumb_up</chef-icon></chef-option>
 *   <chef-option value='opt2'><chef-icon>thumb_down</chef-icon></chef-option>
 * </chef-toggle>
 *
 * @example
 * <script>
 *   const toggle = document.querySelector('#ex1toggle');
 *   toggle.addEventListener('toggle', (event) => alert(event.target.value));
 * </script>
 *
 * <chef-toggle id='ex1toggle'>
 *   <chef-option value='opt1'>Option 1</chef-option>
 *   <chef-option value='opt2'>Option 2</chef-option>
 *   <chef-option value='opt3'>Option 3</chef-option>
 * </chef-toggle>
 *
 * @example
 * <chef-radio>
 *   <chef-option value='opt1'>Option 1</chef-option>
 *   <chef-option value='opt2'>Option 2</chef-option>
 *   <chef-option value='opt3'>Option 3</chef-option>
 * </chef-radio>
 *
 * @example
 * <chef-phat-radio>
 *   <chef-option value='opt1'>Option 1</chef-option>
 *   <chef-option value='opt2'>Option 2</chef-option>
 *   <chef-option value='opt3'>Option 3</chef-option>
 * </chef-phat-radio>
 */
@Component({
  tag: 'chef-option'
})
export class ChefOption {

  /**
   * The value that will be returned when the option is selected.
   */
  @Prop() value = '';

  /**
   * Specifies whether or not this option is selected.
   */
  @Prop() selected = false;

  /**
   * Used to identify different options.
   * This is set automatically so you shouldn't have to change it.
   */
  @Prop() optionId: string;

  @Element() el: HTMLElement;

  private width = 0;

  constructor() {
    id = id + 1;
    this.optionId = this.optionId || `chef-option${id}`;
  }

  @Method() getContent(): string {
    return this.el.querySelector('.option-content').innerHTML;
  }

  @Method() getWidth(): number {
    return this.width;
  }

  componentWillLoad() {
    this.width = this.el.scrollWidth;
  }

  hostData() {
    return {
      role: 'option',
      id: this.optionId,
      class: this.selected ? 'selected' : ''
    };
  }

  render() {
    return [
      <style>
        { `[selected=${this.optionId}] [${this.optionId}], [highlighted=${this.optionId}] #${this.optionId} {
             background-color: hsl(var(--chef-primary-bright));
             color: hsl(var(--chef-white));
           }`
        }
      </style>,
      <span class="option-content">
        <slot />
      </span>
    ];
  }

}
