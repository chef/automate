import { Config } from '@stencil/core';
import { sass } from '@stencil/sass';
import sassLib from 'node-sass';

export const config: Config = {
  namespace: 'chef',
  outputTargets: [
    {
      type: 'www',
      serviceWorker: null // disable service workers
    },
    { type: 'dist' }
  ],
  globalStyle: 'src/global/chef.scss',
  copy:[
    { src: 'global/variables.css', dest: 'styles/variables.example.css' },
    { src: 'global/ui-lib-styles.css', dest: 'styles/ui-lib-styles.css' },
    { src: '../node_modules/material-design-icons/iconfont',
      dest: 'assets/fonts/material-icons' },
    { src: 'sandbox.html', dest: 'sandbox.html' }
  ],
  plugins: [
    sass()
  ],
  testing: {
    browserArgs: ['--no-sandbox', '--disable-setuid-sandbox']
  }
};
