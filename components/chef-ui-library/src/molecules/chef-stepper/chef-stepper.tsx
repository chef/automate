import { Component } from '@stencil/core';

/**
 * @description
 * The `<chef-stepper>` molecule is used to display a list of
 * [`<chef-step>`](/atoms/chef-step) atoms.
 *
 * @example
 * <chef-stepper>
 *   <chef-step selected>Add nodes</chef-step>
 *   <chef-step disabled>Add credentials</chef-step>
 *   <chef-step disabled>Preview nodes</chef-step>
 *   <chef-step disabled>Add profiles</chef-step>
 *   <chef-step disabled>Add scheduler</chef-step>
 * </chef-stepper>
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
 *   <chef-step>Add nodes</chef-step>
 *   <chef-step>Add credentials</chef-step>
 *   <chef-step>Preview nodes</chef-step>
 *   <chef-step>Add profiles</chef-step>
 *   <chef-step selected>Add scheduler</chef-step>
 * </chef-stepper>
 *
 * @example
 * <chef-stepper>
 *   <chef-step>Add nodes</chef-step>
 *   <chef-step>Add credentials</chef-step>
 *   <chef-step selected>Preview nodes</chef-step>
 *   <chef-step>Add profiles</chef-step>
 *   <chef-step>Add scheduler</chef-step>
 * </chef-stepper>
 */
@Component({
  tag: 'chef-stepper',
  styleUrl: 'chef-stepper.scss'
})
export class ChefStepper {

  render() {
    return (
      <slot />
    );
  }
}
