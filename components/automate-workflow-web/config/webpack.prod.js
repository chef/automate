var webpackMerge = require('webpack-merge');
var commonConfig = require('./webpack.common.js');
var DedupePlugin = require('webpack/lib/optimize/DedupePlugin');
var UglifyJsPlugin = require('webpack/lib/optimize/UglifyJsPlugin');
var WebpackMd5Hash = require('webpack-md5-hash');
var DefinePlugin = require('webpack/lib/DefinePlugin');

/**
 * Webpack configuration
 * See: http://webpack.github.io/docs/configuration.html#cli
 */
module.exports = webpackMerge(commonConfig, {

  plugins: [

    // Plugin: WebpackMd5Hash
    // Description: Plugin to replace a standard webpack chunkhash with md5.
    // See: https://www.npmjs.com/package/webpack-md5-hash
    new WebpackMd5Hash(),

    // Plugin: DedupePlugin
    // Description: Prevents the inclusion of duplicate code into your bundle
    // and instead applies a copy of the function at runtime.
    // See: https://webpack.github.io/docs/list-of-plugins.html#defineplugin
    // See: https://github.com/webpack/docs/wiki/optimization#deduplication
    new DedupePlugin(),

    // Plugin: UglifyJsPlugin
    // Description: Minimize all JavaScript output of chunks.
    // Loaders are switched into minimizing mode.
    // See: https://webpack.github.io/docs/list-of-plugins.html#uglifyjsplugin
    // NOTE: To debug prod builds uncomment //debug lines and comment //prod lines
    new UglifyJsPlugin({
      beautify: false,
      mangle: false,
      comments: false,
      compress: {
        screw_ie8: true
      }
    }),

    // Plugin: DefinePlugin
    // Description: Define free variables.
    // Useful for having development builds with debug logging or adding global constants.
    //
    // Environment helpers
    //
    // See: https://webpack.github.io/docs/list-of-plugins.html#defineplugin
    new DefinePlugin({
      'A2URL': JSON.stringify('/')
    })
  ]
});
