import { Component, Prop, h } from '@stencil/core';
import map from 'lodash/fp/map';
import get from 'lodash/fp/get';
import getOr from 'lodash/fp/getOr';
import pipe from 'lodash/fp/pipe';

import { DocEntity } from '../../entities/docs/doc.entity';

@Component({
  tag: 'chef-ui-doc',
  styleUrl: 'chef-ui-doc.scss'
})
export class ChefUIDoc {

  @Prop() doc: DocEntity;

  render() {
    return [
      <h2>{ get('name', this.doc) }</h2>,
      this.summary(this.doc),
      this.description(this.doc),
      this.properties(this.doc),
      <h3>Examples</h3>,
      this.examples(this.doc)
    ];
  }

  private summary(doc) {
    return (
      <div class="summary">
        <ul>
          <li><strong>Tag:</strong> { get('tag', doc) }</li>
          <li><strong>ShadowDom:</strong> { getOr('', 'shadow', doc).toString() }</li>
        </ul>
      </div>
    );
  }

  private description(doc) {
    const text = getOr('No Description', 'description', doc);
    return (
      <div class="description">
        <h3>Description</h3>
        <chef-markdown text={text}></chef-markdown>
      </div>
    );
  }

  private properties(doc) {
    const propRow = (prop) => {
      return (
        <tr>
          <td>{ get('name', prop) }</td>
          <td>{ get('description', prop) }</td>
          <td>{ get('defaultValue', prop) }</td>
        </tr>
      );
    };

    const propertyList = pipe(getOr([], 'properties'),
                              map(propRow))(doc);

    if (propertyList.length > 0) {
      return (
        <div class="properties">
          <h3>Properties</h3>
          <table>
            <thead>
              <tr>
                <th>Name</th>
                <th>Description</th>
                <th>Default Value</th>
              </tr>
            </thead>
            <tbody>
              { propertyList }
            </tbody>
          </table>
        </div>
      );
    } else {
      return '';
    }
  }

  private examples(doc) {
    const example = (ex) => {
      return (
        <chef-example code={ ex }></chef-example>
      );
    };

    return pipe(getOr([], 'examples'),
                map(example))(doc);
  }
}
