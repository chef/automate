var sassLib = require('node-sass');
var { sass } = require('@stencil/sass');

exports.config = {
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
    sass({
      functions: {
        'chef-hsl($hueSatLight)': function (hsl) {
          return new sassLib.types.String(`hsl(${hsl.getValue()})`);
        },
        'chef-hsla($hueSatLight, $alpha: 1)': function (hsl, alpha) {
          return new sassLib.types.String(`hsla(${hsl.getValue()}, ${alpha.getValue()})`);
        }
      }
    })
  ],
  testing: {
    browserArgs: ['--no-sandbox', '--disable-setuid-sandbox']
  }
};
