import { Component, Prop, State } from '@stencil/core';
import { Store } from '@stencil/redux';
import get from 'lodash/fp/get';
import getOr from 'lodash/fp/getOr';
import map from 'lodash/fp/map';
import first from 'lodash/fp/first';
import startCase from 'lodash/fp/startCase';
import pipe from 'lodash/fp/pipe';
import filter from 'lodash/fp/filter';

import { IndexedEntities } from '../../entities/entities';
import { DocEntity } from '../../entities/docs/doc.entity';

@Component({
  tag: 'chef-ui-docs',
  styleUrl: 'chef-ui-docs.scss'
})
export class ChefUIDocs {

  @Prop({ context: 'store' }) store: Store;
  @Prop() match: any;
  @Prop() docType: string;

  @State() docs: IndexedEntities<DocEntity>;
  @State() docIds: string[];

  componentWillLoad() {
    this.store.mapStateToProps(this, (state) => {
      const docs = get(['docs', 'byId'], state);
      const docIds =
        pipe(get(['docs', 'allIds']),
             filter((id: string) => get([id, 'docType'], docs) === this.docType))(state);
      return {
        docs,
        docIds
      };
    });
  }

  render() {
    const currentDoc = getOr(first(this.docIds),
                             ['params', 'id'],
                             this.match);

    return (
      <chef-layout>
        <chef-aside id="sidebar-nav">
          <nav aria-labelledby="nav2">
          <h2 id="nav2" class="visually-hidden">Atom Navigation</h2>
            <ul>
              { this.nav(this.docType, this.docIds, this.docs) }
            </ul>
          </nav>
        </chef-aside>

        <chef-main class="doc">
          <chef-ui-doc doc={ get(currentDoc, this.docs) }></chef-ui-doc>
        </chef-main>
      </chef-layout>
    );
  }

  private nav(docType, docNames, docs) {
    const link = (doc) => {
      return (
        <li>
          <stencil-route-link url={`${docType}/${doc}`}>{ pipe(get([doc, 'name']), startCase)(docs) }</stencil-route-link>
        </li>
      );
    };

    return map(link, docNames);
  }

}
