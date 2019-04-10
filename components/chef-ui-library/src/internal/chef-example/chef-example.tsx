import { Component, Element, Prop } from '@stencil/core';

/**
 * @description
 * The `<chef-example>` molecule is used to display syntax-highlighted example code alongside their
 * rendered output.
 *
 * @example
 * <chef-example code="<h1>Highlight me</h1>"></chef-example>
 */
@Component({
  tag: 'chef-example',
  styleUrl: 'chef-example.scss'
})
export class ChefExample {

  @Element() el: HTMLElement;

  /**
   * The example code to be presented.
   */
  @Prop() code: string;

  componentDidLoad() {
    this.execScripts();
  }

  componentDidUpdate() {
    this.execScripts();
  }

  render() {
    return ([
      <chef-snippet class="example-code" code={ this.code }></chef-snippet>,
      <div class="example-display" innerHTML={ this.code }></div>
    ]);
  }

  private execScripts() {
    const scripts = Array.from(this.el.querySelectorAll('script'));
    return scripts.map(script => new Function(script.innerText)());
  }
}
