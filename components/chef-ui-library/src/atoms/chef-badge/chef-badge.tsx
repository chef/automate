import { Component, Prop, h } from '@stencil/core';

/**
 * @description
 * `<chef-badge>` is used to Badges are generally used to provide context or emphasis on a
 * characteristic of another object in the interface, e.g., the channel name for a Habitat
 * package. They are generally actionless. But when there are more descriptions needed to
 * help explain the context, a hover state with tooltip can be added.
 *
 * Semantic colors can be applied to indicate: general, primary, critical, warning, and success.
 *
 * @example
 * <chef-badge>general</chef-badge>
 * <chef-badge primary>primary</chef-badge>
 * <chef-badge critical>critical</chef-badge>
 * <chef-badge warning>warning</chef-badge>
 * <chef-badge success>success</chef-badge>
 *
 * <chef-separator></chef-separator>
 *
 * <chef-badge no-data>general</chef-badge>
 * <chef-badge no-data primary>primary</chef-badge>
 * <chef-badge no-data critical>critical</chef-badge>
 * <chef-badge no-data warning>warning</chef-badge>
 * <chef-badge no-data success>success</chef-badge>
 *
 * @example
 * <chef-badge id="general-tooltip" tooltip="Tooltip">general</chef-badge>
 * <chef-badge primary id="primary-tooltip" tooltip="Tooltip">primary</chef-badge>
 * <chef-badge critical id="critical-tooltip" tooltip="Tooltip">critical</chef-badge>
 * <chef-badge warning id="warning-tooltip" tooltip="Tooltip">warning</chef-badge>
 * <chef-badge success id="success-tooltip" tooltip="Tooltip">success</chef-badge>
 *
 * <chef-separator></chef-separator>
 *
 * <chef-badge no-data id="general-2-tooltip" tooltip="Tooltip">general</chef-badge>
 * <chef-badge no-data primary id="primary-2-tooltip" tooltip="Tooltip">primary</chef-badge>
 * <chef-badge no-data critical id="critical-2-tooltip" tooltip="Tooltip">critical</chef-badge>
 * <chef-badge no-data warning id="warning-2-tooltip" tooltip="Tooltip">warning</chef-badge>
 * <chef-badge no-data success id="success-2-tooltip" tooltip="Tooltip">success</chef-badge>
 */
@Component({
  tag: 'chef-badge',
  styleUrl: 'chef-badge.scss'
})
export class ChefBadge {

  /**
   * Indicate badge has no data
   */
  @Prop({ reflectToAttr: true }) noData = false;

  /**
   * The ID of the element to attach the tooltip
   */
  @Prop({ reflectToAttr: true }) id: string;

  /**
   * Text to be displayed within tooltips
   */
  @Prop({ reflectToAttr: true }) tooltip: string;

  render() {
    const tooltip = this.tooltip ? <chef-tooltip for={this.id}>{this.tooltip}</chef-tooltip> : null;
    const badge = (
      <div class="badge" id={this.id} no-data={this.noData}>
        <slot />
      </div>
    );

    return [badge, tooltip];
  }
}
