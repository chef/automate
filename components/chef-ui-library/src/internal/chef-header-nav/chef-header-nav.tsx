import { Component, h } from '@stencil/core';

@Component({
  tag: 'chef-header-nav',
  styleUrl: 'chef-header-nav.scss'
})
export class ChefHeaderNav {

  render() {
    return (
    <header role="banner">
      <nav aria-labelledby="nav1" class="navigation-wrapper">
        <div class="left-nav" role="menubar">
          <stencil-route-link role="menuitem" class="logo-link" title="Chef UI Library Home Link ReadMe" url="/" >
            <h2>Chef UI Library</h2>
          </stencil-route-link>
          <div class="navigation-menu">
            <h2 id="nav1" class="visually-hidden">Main Header Navigation for UI Library</h2>
            { /* <stencil-route-link role="menuitem" class="nav-link" tabindex="0" url="/">Overview</stencil-route-link> */ }
            <stencil-route-link role="menuitem" class="nav-link" tabindex="0" url="/design">Design Elements</stencil-route-link>
            <stencil-route-link role="menuitem" class="nav-link" tabindex="0" url="/atoms">Atoms</stencil-route-link>
            <stencil-route-link role="menuitem" class="nav-link" tabindex="0" url="/molecules">Molecules</stencil-route-link>
            <stencil-route-link role="menuitem" class="nav-link" tabindex="0" url="/templates">Templates</stencil-route-link>
            <stencil-route-link role="menuitem" class="nav-link" tabindex="0" url="/charts">Charts</stencil-route-link>
            {/* <stencil-route-link role="menuitem" class="nav-link" tabindex="0" url="/">CSS Utilities</stencil-route-link> */}
          </div>
        </div>
      </nav>
    </header>
    );
  }

}
