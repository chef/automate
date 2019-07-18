import { Component, h } from '@stencil/core';

@Component({
  tag: 'chef-read-me',
  styleUrl: 'chef-read-me.scss'
})
export class ChefReadMe {

  render() {
    return (
    <chef-layout>
      <chef-header></chef-header>
      <chef-main>
        <div id="readme-wrap">
          <h1>About the Chef UI &amp; Pattern Library</h1>
           <p>Our Pattern Library is a byproduct of our move to a more
           responsive, nimble, &amp; intuitive app. We've worked to distill most of
           the Automate interface into a set of atomic pieces, forming the
           pattern library you see here. By documenting and assembling a
           reference site of our patterns, we are able to speed up our process
           and solve some internal communication problems. A common lexicon of
           code and UI elements benefits us in a few ways: </p>

          <ul>
             <li>We can build consistently, focusing our energy on workflows and
             logic, not web forms and list items.</li>
             <li>We can reuse code instead of reinventing the wheel or roping in
              an engineer.</li>
             <li>We can see all of our patterns in one place, quickly revealing
             maintenance issues.</li>
             <li>We can bake in accessibility</li>
          </ul>

          <h2>What is StencilJS? What are web components? What is Atomic Design?</h2>
           <p>tl;dr StencilJS is a way to build web components - web components can be wired into
           any framework or code where they need to be used. This will future
           proof the component library so we wouldn't have to throw everything
           away if we moved away from Angular. And in the meantime, we can
           continue to work in Angular and the handful of components in our
           Angular Component Library as well. In regards to Atomic Design, we
           will refer to the formerly named “UI Components” with their new
           Atomic Design names such as Atoms and Molecules from now on since we
           use the word component for more than a dozen different concepts
           throughout Angular and our code base.</p>
           <p><a href="https://stenciljs.com/docs/intro">StencilJS Docs</a></p>
           <p><a href="http://atomicdesign.bradfrost.com/">Atomic Design</a></p>
        </div>
      </chef-main>
    </chef-layout>
    );
  }

}
