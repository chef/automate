import { Component, Prop } from '@stencil/core';
import marked from 'marked';

/**
 * Have the renderer display code blocks as `<chef-snippet>` tags instead
 * of `<pre>` tags to keep styling of code blocks consistent.
 */
const renderer = Object.assign(new marked.Renderer(), {
  code: (code: string, lang: string) => {
    return `<chef-snippet code="${code.replace(/"/g, '&quot;')}" lang="${lang}"></chef-snippet>`;
  }
});

/**
 * @description
 * The `<chef-markdown>` atom is used to parse and display markdown text.
 *
 * @example
 * <chef-markdown text="# Heading with a [link](#to-nothing)"></chef-markdown>
 *
 * @example
 * <textarea id='md-editor' placeholder='Type some markdown...'></textarea>
 * <chef-markdown id='md-display'></chef-markdown>
 *
 * <style>
 *  #md-editor { width: 100%; min-height: 10em; }
 * </style>
 *
 * <script>
 *   const editor = document.getElementById('md-editor');
 *   const display = document.getElementById('md-display');
 *   editor.addEventListener('input', e => display.text = e.target.value);
 * </script>
 */
@Component({
  tag: 'chef-markdown',
  styleUrl: 'chef-markdown.scss'
})
export class ChefMarkdown {
  /**
   * The markdown text to be parsed.
   */
  @Prop() text = '';

  render() {
    const parsed = this.parse(this.text);
    return (
      <div innerHTML={ parsed }></div>
    );
  }

  private parse(text: string): string {
    return marked.parse(text, { renderer });
  }
}
