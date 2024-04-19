import { Config } from '@stencil/core';
import { sass } from '@stencil/sass';
import sassLib from 'node-sass'; //Anees: Need to remove this.

const copy = [
  { src: 'global/variables.css', dest: 'styles/variables.example.css' },
  { src: 'global/ui-lib-styles.css', dest: 'styles/ui-lib-styles.css' },
  { src: 'assets', dest: 'assets' },
  {
    src: '../node_modules/material-design-icons-iconfont/dist/',
    dest: 'assets/fonts/material-icons'
  },
  { src: 'sandbox.html', dest: 'sandbox.html' }
];

export const config: Config = {
  namespace: 'chef',
  outputTargets: [
    {
      type: 'www',
      copy,
      serviceWorker: null // disable service workers
    },
    {
      type: 'dist',
      copy,
    },
  ],
  globalStyle: 'src/global/chef.scss',
  plugins: [
    sass()
  ],
  testing: {
    browserArgs: ['--no-sandbox', '--disable-setuid-sandbox']
  }
};
