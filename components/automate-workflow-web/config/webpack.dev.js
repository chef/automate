var webpackMerge = require('webpack-merge'); //Used to merge webpack configs
var commonConfig = require('./webpack.common.js'); //The settings that are common to prod and dev
var DefinePlugin = require('webpack/lib/DefinePlugin');

/**
 * Webpack configuration
 * See: http://webpack.github.io/docs/configuration.html#cli
 */
module.exports = webpackMerge(commonConfig, {

  // Switch loaders to debug mode.
  // See: http://webpack.github.io/docs/configuration.html#debug
  debug: true,

  plugins: [
    new DefinePlugin({
      'A2URL': JSON.stringify('/a2/placeholder.html')
    })
  ],

  node: {
    global: 'window',
    crypto: 'empty',
    process: true,
    module: false,
    clearImmediate: false,
    setImmediate: false
  }
});
