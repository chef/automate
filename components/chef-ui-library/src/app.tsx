// Collections are now defined as imports in the main component. Since they
// cause side effects we need to disable that rule in this case.
/* tslint:disable:no-import-side-effect */
import '@stencil/router';
/* tslint:enable:no-import-side-effect */

import { Component, Prop } from '@stencil/core';
import { Store } from '@stencil/redux';

import { configureStore } from './store';
import { getDocs } from './entities/docs/doc.actions';

@Component({
  tag: 'chef-library-app'
})
export class ChefLibraryApp {

  @Prop({ context: 'store' }) store: Store;

  getDocs: typeof getDocs;

  componentWillLoad() {
    this.store.setStore(configureStore({}));
    this.store.mapDispatchToProps(this, {
      getDocs
    });
  }

  componentDidLoad() {
    this.getDocs();
  }

  render() {
    return (
      <div>
        <chef-header-nav></chef-header-nav>
        <stencil-router>
          <stencil-route
            url="/"
            component="chef-read-me"
            exact={true}></stencil-route>
          <stencil-route
            url={['/design/:id', '/design']}
            componentProps={{ docType: 'design' }}
            component="chef-design-docs"></stencil-route>
          <stencil-route
            url={['/atoms/:id', '/atoms']}
            componentProps={{ docType: 'atoms' }}
            component="chef-ui-docs"></stencil-route>
          <stencil-route
            url={['/molecules/:id', '/molecules']}
            componentProps={{ docType: 'molecules' }}
            component="chef-ui-docs"></stencil-route>
          <stencil-route
            url={['/templates/:id', '/templates']}
            componentProps={{ docType: 'templates' }}
            component="chef-ui-docs"></stencil-route>
          <stencil-route
            url={['/charts/:id', '/charts']}
            componentProps={{ docType: 'charts' }}
            component="chef-ui-docs"></stencil-route>
        </stencil-router>
      </div>
    );
  }
}
