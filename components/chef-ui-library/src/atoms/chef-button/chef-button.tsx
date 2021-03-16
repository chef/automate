import { Component, Host, Prop, h } from '@stencil/core';

/**
 * @description
 * `<chef-button>` is used to display buttons. It has three style variants:
 *
 * - `primary`
 * - `secondary`
 * - `tertiary`
 *
 * Adding `caution` will modify these style variants to their cautious version.
 *
 * @example
 * <chef-button primary>
 *   Primary
 * </chef-button>
 * <chef-button primary>
 *   <chef-icon>alarm</chef-icon>
 * </chef-button>
 *
 * @example
 * <chef-button primary>Button Text</chef-button>
 * <chef-button primary disabled>Button Text</chef-button>
 * <chef-button primary caution>Button Text</chef-button>
 * <chef-button primary caution disabled>Button Text</chef-button>
 *
 * <chef-separator></chef-separator>
 *
 * <chef-button secondary>Button Text</chef-button>
 * <chef-button secondary disabled>Button Text</chef-button>
 * <chef-button secondary caution>Button Text</chef-button>
 * <chef-button secondary caution disabled>Button Text</chef-button>
 *
 * <chef-separator></chef-separator>
 *
 * <chef-button tertiary>Button Text</chef-button>
 * <chef-button tertiary disabled>Button Text</chef-button>
 * <chef-button tertiary caution>Button Text</chef-button>
 * <chef-button tertiary caution disabled>Button Text</chef-button>
 *
 * @example
 * <chef-button primary>
 *   <chef-icon>add</chef-icon>
 * </chef-button>
 * <chef-button primary disabled>
 *   <chef-icon>add</chef-icon>
 * </chef-button>
 * <chef-button primary caution>
 *   <chef-icon>remove</chef-icon>
 * </chef-button>
 * <chef-button primary caution disabled>
 *   <chef-icon>remove</chef-icon>
 * </chef-button>
 *
 * <chef-separator></chef-separator>
 *
 * <chef-button secondary>
 *   <chef-icon>add</chef-icon>
 * </chef-button>
 * <chef-button secondary disabled>
 *   <chef-icon>add</chef-icon>
 * </chef-button>
 * <chef-button secondary caution>
 *   <chef-icon>remove</chef-icon>
 * </chef-button>
 * <chef-button secondary caution disabled>
 *   <chef-icon>remove</chef-icon>
 * </chef-button>
 *
 * <chef-separator></chef-separator>
 *
 * <chef-button tertiary>
 *   <chef-icon>add</chef-icon>
 * </chef-button>
 * <chef-button tertiary disabled>
 *   <chef-icon>add</chef-icon>
 * </chef-button>
 * <chef-button tertiary caution>
 *   <chef-icon>remove</chef-icon>
 * </chef-button>
 * <chef-button tertiary caution disabled>
 *   <chef-icon>remove</chef-icon>
 * </chef-button>
 *
 * @example
 * <chef-button primary>
 *   <chef-icon>close</chef-icon>
 *   <span>Button Text</span>
 * </chef-button>
 * <chef-button primary disabled>
 *   <chef-icon>close</chef-icon>
 *   <span>Button Text</span>
 * </chef-button>
 * <chef-button primary caution>
 *   <chef-icon>close</chef-icon>
 *   <span>Button Text</span>
 * </chef-button>
 * <chef-button primary caution disabled>
 *   <chef-icon>close</chef-icon>
 *   <span>Button Text</span>
 * </chef-button>
 *
 * <chef-separator></chef-separator>
 *
 * <chef-button secondary>
 *   <chef-icon>close</chef-icon>
 *   <span>Button Text</span>
 * </chef-button>
 * <chef-button secondary disabled>
 *   <chef-icon>close</chef-icon>
 *   <span>Button Text</span>
 * </chef-button>
 * <chef-button secondary caution>
 *   <chef-icon>close</chef-icon>
 *   <span>Button Text</span>
 * </chef-button>
 * <chef-button secondary caution disabled>
 *   <chef-icon>close</chef-icon>
 *   <span>Button Text</span>
 * </chef-button>
 *
 * <chef-separator></chef-separator>
 *
 * <chef-button tertiary>
 *   <chef-icon>close</chef-icon>
 *   <span>Button Text</span>
 * </chef-button>
 * <chef-button tertiary disabled>
 *   <chef-icon>close</chef-icon>
 *   <span>Button Text</span>
 * </chef-button>
 * <chef-button tertiary caution>
 *   <chef-icon>close</chef-icon>
 *   <span>Button Text</span>
 * </chef-button>
 * <chef-button tertiary caution disabled>
 *   <chef-icon>close</chef-icon>
 *   <span>Button Text</span>
 * </chef-button>
 *
 * @example
 * <chef-button primary>
 *   <span>Button Text</span>
 *   <chef-icon>chevron_right</chef-icon>
 * </chef-button>
 * <chef-button primary disabled>
 *   <span>Button Text</span>
 *   <chef-icon>chevron_right</chef-icon>
 * </chef-button>
 * <chef-button primary caution>
 *   <span>Button Text</span>
 *   <chef-icon>chevron_right</chef-icon>
 * </chef-button>
 * <chef-button primary caution disabled>
 *   <span>Button Text</span>
 *   <chef-icon>chevron_right</chef-icon>
 * </chef-button>
 *
 * <chef-separator></chef-separator>
 *
 * <chef-button secondary>
 *   <span>Button Text</span>
 *   <chef-icon>chevron_right</chef-icon>
 * </chef-button>
 * <chef-button secondary disabled>
 *   <span>Button Text</span>
 *   <chef-icon>chevron_right</chef-icon>
 * </chef-button>
 * <chef-button secondary caution>
 *   <span>Button Text</span>
 *   <chef-icon>chevron_right</chef-icon>
 * </chef-button>
 * <chef-button secondary caution disabled>
 *   <span>Button Text</span>
 *   <chef-icon>chevron_right</chef-icon>
 * </chef-button>
 *
 * <chef-separator></chef-separator>
 *
 * <chef-button tertiary>
 *   <span>Button Text</span>
 *   <chef-icon>chevron_right</chef-icon>
 * </chef-button>
 * <chef-button tertiary disabled>
 *   <span>Button Text</span>
 *   <chef-icon>chevron_right</chef-icon>
 * </chef-button>
 * <chef-button tertiary caution>
 *   <span>Button Text</span>
 *   <chef-icon>chevron_right</chef-icon>
 * </chef-button>
 * <chef-button tertiary caution disabled>
 *   <span>Button Text</span>
 *   <chef-icon>chevron_right</chef-icon>
 * </chef-button>
 *
 * @example
 * <chef-button primary>
 *   <chef-icon>bookmark</chef-icon>
 *   <span>Button Text</span>
 *   <chef-icon>close</chef-icon>
 * </chef-button>
 * <chef-button primary disabled>
 *   <chef-icon>bookmark</chef-icon>
 *   <span>Button Text</span>
 *   <chef-icon>close</chef-icon>
 * </chef-button>
 * <chef-button primary caution>
 *   <chef-icon>bookmark</chef-icon>
 *   <span>Button Text</span>
 *   <chef-icon>close</chef-icon>
 * </chef-button>
 * <chef-button primary caution disabled>
 *   <chef-icon>bookmark</chef-icon>
 *   <span>Button Text</span>
 *   <chef-icon>close</chef-icon>
 * </chef-button>
 *
 * <chef-separator></chef-separator>
 *
 * <chef-button secondary>
 *   <chef-icon>bookmark</chef-icon>
 *   <span>Button Text</span>
 *   <chef-icon>close</chef-icon>
 * </chef-button>
 * <chef-button secondary disabled>
 *   <chef-icon>bookmark</chef-icon>
 *   <span>Button Text</span>
 *   <chef-icon>close</chef-icon>
 * </chef-button>
 * <chef-button secondary caution>
 *   <chef-icon>bookmark</chef-icon>
 *   <span>Button Text</span>
 *   <chef-icon>close</chef-icon>
 * </chef-button>
 * <chef-button secondary caution disabled>
 *   <chef-icon>bookmark</chef-icon>
 *   <span>Button Text</span>
 *   <chef-icon>close</chef-icon>
 * </chef-button>
 *
 * <chef-separator></chef-separator>
 *
 * <chef-button tertiary>
 *   <chef-icon>bookmark</chef-icon>
 *   <span>Button Text</span>
 *   <chef-icon>close</chef-icon>
 * </chef-button>
 * <chef-button tertiary disabled>
 *   <chef-icon>bookmark</chef-icon>
 *   <span>Button Text</span>
 *   <chef-icon>close</chef-icon>
 * </chef-button>
 * <chef-button tertiary caution>
 *   <chef-icon>bookmark</chef-icon>
 *   <span>Button Text</span>
 *   <chef-icon>close</chef-icon>
 * </chef-button>
 * <chef-button tertiary caution disabled>
 *   <chef-icon>bookmark</chef-icon>
 *   <span>Button Text</span>
 *   <chef-icon>close</chef-icon>
 * </chef-button>
 *
 * @example
 * <chef-button primary>
 *   <chef-icon>save</chef-icon>
 *   <span>Button Text</span>
 * </chef-button>
 * <chef-button primary disabled>
 *   <chef-loading-spinner></chef-loading-spinner>
 *   <span>Button Text</span>
 * </chef-button>
 * <chef-button primary caution>
 *   <chef-icon>delete</chef-icon>
 *   <span>Button Text</span>
 * </chef-button>
 * <chef-button primary caution disabled>
 *   <chef-loading-spinner></chef-loading-spinner>
 *   <span>Button Text</span>
 * </chef-button>
 *
 * <chef-separator></chef-separator>
 *
 * <chef-button secondary>
 *   <chef-icon>save</chef-icon>
 *   <span>Button Text</span>
 * </chef-button>
 * <chef-button secondary disabled>
 *   <chef-loading-spinner></chef-loading-spinner>
 *   <span>Button Text</span>
 * </chef-button>
 * <chef-button secondary caution>
 *   <chef-icon>delete</chef-icon>
 *   <span>Button Text</span>
 * </chef-button>
 * <chef-button secondary caution disabled>
 *   <chef-loading-spinner></chef-loading-spinner>
 *   <span>Button Text</span>
 * </chef-button>
 *
 * <chef-separator></chef-separator>
 *
 * <chef-button tertiary>
 *   <chef-icon>save</chef-icon>
 *   <span>Button Text</span>
 * </chef-button>
 * <chef-button tertiary disabled>
 *   <chef-loading-spinner></chef-loading-spinner>
 *   <span>Button Text</span>
 * </chef-button>
 * <chef-button tertiary caution>
 *   <chef-icon>delete</chef-icon>
 *   <span>Button Text</span>
 * </chef-button>
 * <chef-button tertiary caution disabled>
 *   <chef-loading-spinner></chef-loading-spinner>
 *   <span>Button Text</span>
 * </chef-button>
 *
 * @example
 * <chef-button secondary disabled>
 *   <chef-icon>chevron_left</chef-icon>
 * </chef-button>
 * <chef-button primary>1</chef-button>
 * <chef-button secondary>2</chef-button>
 * <chef-button secondary>3</chef-button>
 * <chef-button secondary>
 *   <chef-icon>chevron_right</chef-icon>
 * </chef-button>
 *
 * <chef-separator></chef-separator>
 *
 * <chef-button secondary>
 *   <chef-icon>chevron_left</chef-icon>
 * </chef-button>
 * <chef-button secondary>1</chef-button>
 * <chef-button primary>2</chef-button>
 * <chef-button secondary>3</chef-button>
 * <chef-button secondary>
 *   <chef-icon>chevron_right</chef-icon>
 * </chef-button>
 *
 * <chef-separator></chef-separator>
 *
 * <chef-button secondary>
 *   <chef-icon>chevron_left</chef-icon>
 * </chef-button>
 * <chef-button secondary>1</chef-button>
 * <chef-button secondary>2</chef-button>
 * <chef-button primary>3</chef-button>
 * <chef-button secondary disabled>
 *   <chef-icon>chevron_right</chef-icon>
 * </chef-button>
 *
 * @example
 * <chef-button type="reset" tertiary>Cancel</chef-button>
 * <chef-button type="button" secondary>Save Draft</chef-button>
 * <chef-button type="submit" primary>Submit</chef-button>
 *
 * @example
 * <chef-button url="/" primary>Link!</chef-button>
 * <chef-button url="/" target="_blank" secondary>Link in a new tab!</chef-button>
 */
@Component({
  tag: 'chef-button',
  styleUrl: 'chef-button.scss'
})
export class ChefButton {

  /**
   * The button type (either 'button', 'submit', or 'reset')
   */
  @Prop({ reflectToAttr: true }) type: 'submit' | 'reset' | 'button' = 'button';

  /**
   * Name for form to submit
   */
  @Prop({ reflectToAttr: true }) form: string;

  /**
   * Create a primary button
   */
  @Prop({ reflectToAttr: true }) primary = false;

  /**
   * Create a secondary button
   */
  @Prop({ reflectToAttr: true }) secondary = false;

  /**
   * Create a tertiary button
   */
  @Prop({ reflectToAttr: true }) tertiary = false;

  /**
   * Indicate button as potentially dangerous
   */
  @Prop({ reflectToAttr: true }) caution = false;

  /**
   * Indicate button as disabled
   */
  @Prop({ reflectToAttr: true }) disabled = false;

  /**
   * Url for links that look like buttons
   */
  @Prop({ reflectToAttr: true }) url: string;

  /**
   * Target for links
   */
  @Prop({ reflectToAttr: true }) target: string;

  @Prop({ reflectToAttr: true }) firstFocus = false;

  render() {
    const link = (
      <a href={this.url} target={this.target}>
        <slot />
      </a>
    );

    const button = (
      <button
        type={this.type}
        form={this.form}
        disabled={this.disabled}
        aria-disabled={this.disabled}>
        <slot />
      </button>
    );

    return (
      <Host aria-disabled={this.disabled}>
        {this.url ? link : button}
      </Host>
    );
  }
}
