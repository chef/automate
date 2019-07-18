import { Component, Event, EventEmitter, Prop, h } from '@stencil/core';

/**
 * @description
 * chef-data-point is used in conjunction with various charts to specify data points.
 *
 * @example
 * <style>
 *   chef-radial-chart .failed, chef-radial-chart .critical {
 *     color: var(--chef-critical);
 *   }
 *  chef-radial-chart .warning, chef-radial-chart .warning {
 *     color: var(--chef-primary);
 *   }
 *   chef-radial-chart .success, chef-radial-chart .major {
 *     color: var(--chef-ok);
 *   }
 *   chef-radial-chart .skipped, chef-radial-chart .minor {
 *     color: var(--chef-unknown);
 *   }
 * </style>
 * <chef-radial-chart style="width: 220px; height: 220px;">
 *   <span slot="innerText">Text for the center of the chart</span>
 *
 *   <chef-data-point value="4" class="failed">4 Failed</chef-data-point>
 *   <chef-data-point value="3" class="warning">3 Failed</chef-data-point>
 *   <chef-data-point value="2" class="success">2 Successful</chef-data-point>
 *   <chef-data-point value="1" class="skipped">1 Skipped</chef-data-point>
 *
 *   <chef-data-point value="4" secondary class="critical">Critical</chef-data-point>
 *   <chef-data-point value="3" secondary class="warning">Warning</chef-data-point>
 *   <chef-data-point value="2" secondary class="major">Major</chef-data-point>
 *   <chef-data-point value="1" secondary class="minor">Minor</chef-data-point>
 * </chef-radial-chart>
 */
@Component({
  tag: 'chef-data-point'
})
export class ChefDataPoint {

  /**
   * The value assigned to the data point.
   */
  @Prop() value = 0;

  /**
   * Some charts support special secondary data points.
   */
  @Prop() secondary = false;

  @Event() updated: EventEmitter;

  componentWillUpdate() {
    this.updated.emit();
  }

  render() {
    return (
      <slot />
    );
  }

}
