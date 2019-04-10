import { Component, Prop } from '@stencil/core';

/**
 * @description
 * The `<chef-step>` atom is used to define a step within a
 * [`<chef-stepper>`](/molecules/chef-stepper) molecule.
 *
 * @example
 * <chef-stepper>
 *   <chef-step>Add nodes</chef-step>
 *   <chef-step>Add credentials</chef-step>
 *   <chef-step selected>Preview nodes</chef-step>
 *   <chef-step disabled>Add profiles</chef-step>
 *   <chef-step disabled>Add scheduler</chef-step>
 * </chef-stepper>
 *
 * @example
 * <chef-stepper>
 *   <chef-step>
 *     <chef-icon>check</chef-icon>
 *     <a href="">Add nodes</a>
 *   </chef-step>
 *   <chef-step>
 *     <chef-icon>check</chef-icon>
 *     <a href="">Add credentials</a>
 *   </chef-step>
 *   <chef-step selected>
 *     <a href="">Preview nodes</a>
 *   </chef-step>
 *   <chef-step disabled>
 *     <a href="">Add profiles</a>
 *   </chef-step>
 *   <chef-step disabled>
 *     <a href="">Add scheduler</a>
 *   </chef-step>
 * </chef-stepper>
 */
@Component({
  tag: 'chef-step',
  styleUrl: 'chef-step.scss'
})
export class ChefStep {

  /**
   * Indicates that the step is disabled.
   */
  @Prop({ reflectToAttr: true }) disabled = false;

  /**
   * Indicates that the step is selected.
   */
  @Prop({ reflectToAttr: true }) selected = false;

  render() {
    return (
      <slot />
    );
  }
}
