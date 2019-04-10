import { Component } from '@stencil/core';

/**
 * @description
 * Displays text in pill form. Used for tagging things and whatnot.
 * You can set the background color and text color with css when using
 * the atom directly. When the atom is part of another molecule there
 * are two css variables provided.
 * --pill-background-color
 * --pill-text-color
 *
 * @example
 * <chef-pill>Some Pill Text</chef-pill>
 *
 * @example
 * <chef-pill style="background-color: blue; color: white">Some Pill Text</chef-pill>
 *
 * @example
 * <div style="--pill-background-color: black; --pill-text-color: white">
 *   <chef-input type='key-value' value='foobar:bizbang'></chef-input>
 * </div>
 */
@Component({
  tag: 'chef-pill',
  styleUrl: './chef-pill.scss'
})
export class ChefPill {
  render() {
    return <slot />;
  }
}
