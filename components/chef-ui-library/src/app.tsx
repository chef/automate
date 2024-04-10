import { Component, h } from '@stencil/core';

import { store } from './store';
import { getDocs } from './entities/docs/doc.actions';

@Component({
  tag: 'chef-library-app'
})
export class ChefLibraryApp {

  // getDocs: typeof getDocs;

  // componentWillLoad() {
  //   // store.setStore(store);
  //   store.dispatch(getDocs());
  // }

  componentDidLoad() {
    // store.dispatch(getDocs());
    
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
