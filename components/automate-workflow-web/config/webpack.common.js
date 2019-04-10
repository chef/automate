var webpack = require('webpack');
var helpers = require('./helpers');
var CopyWebpackPlugin = require('copy-webpack-plugin');
var HtmlWebpackPlugin = require('html-webpack-plugin');

var METADATA = {
  title: 'Chef Automate'
};

/**
 * Webpack configuration
 * See: http://webpack.github.io/docs/configuration.html#cli
 */
module.exports = {

  // Static metadata for index.html
  metadata: METADATA,

  // Developer tool to enhance debugging
  // See: http://webpack.github.io/docs/configuration.html#devtool
  // See: https://github.com/webpack/docs/wiki/build-performance#sourcemaps
  devtool: 'source-map',

  // Cache generated modules and chunks to improve performance for multiple incremental builds.
  // This is enabled by default in watch mode.
  // You can pass false to disable it.
  // See: http://webpack.github.io/docs/configuration.html#cache

  // The entry point for the bundle
  // See: http://webpack.github.io/docs/configuration.html#entry
  entry: {
    'fonts': 'font-awesome-loader',
    'main': './src/main.js'
  },

  // Options affecting the resolving of modules.
  // See: http://webpack.github.io/docs/configuration.html#resolve
  resolve: {

    // An array of extensions that should be used to resolve modules.
    // See: http://webpack.github.io/docs/configuration.html#resolve-extensions
    extensions: ['', '.js', '.ts'],

    root: helpers.root('src'),

    modulesDirectories: [
      'node_modules',
      'src/configs'
    ],

    alias: {
      spin: 'spin.js'
    }
  },

  // Options affecting the normal modules.
  // See: http://webpack.github.io/docs/configuration.html#module
  module: {

    // IMPORTANT: The loaders here are resolved relative to the resource which they are applied to.
    // This means they are not resolved relative to the configuration file.
    // See: http://webpack.github.io/docs/configuration.html#module-loaders
    loaders: [

      // Loader support for TypeScript (.ts) files
      // See: https://github.com/s-panferov/awesome-typescript-loader
      {
        test: /\.ts$/,
        loader: 'awesome-typescript-loader',
        exclude: [/\.(spec|e2e)\.ts$/]
      },

      // Babel loader
      {
        test:   /\.js/,
        loader: 'babel',
        exclude: /node_modules/
      },

      // Loader support for *.css files
      // Returns file content as string
      // See: https://github.com/webpack/css-loader
      {
        test: /\.css$/,
        loader: 'style-loader!css-loader'
      },

      // Raw loader support for *.scss files
      // Returns file content as string
      // See: https://github.com/webpack/raw-loader
      // See: https://github.com/jtangelder/sass-loader
      {
        test: /\.scss$/,
        exclude: /node_modules/,
        loaders: ['style', 'css', 'resolve-url', 'sass?sourceMap']
      },

      // Raw loader support for *.html
      // Returns file content as string
      // See: https://github.com/webpack/raw-loader
      {
        test: /\.html$/,
        loader: 'raw-loader',
        exclude: [helpers.root('src/pages/index.html')]
      },

      // Fonts and images
      {
        test: /\.(ttf|eot|svg|png|jpg|woff2?|otf)(\?[\s\S]+)?$/,
        loader: 'url'
      }
    ]
  },

  // Options affecting the output of the compilation.
  // See: http://webpack.github.io/docs/configuration.html#output
  output: {

    publicPath: '/workflow/javascripts',

    // The output directory as absolute path (required).
    // See: http://webpack.github.io/docs/configuration.html#output-path
    path: helpers.root('dist/javascripts'),

    // Specifies the name of each output file on disk.
    // See: http://webpack.github.io/docs/configuration.html#output-filename
    filename: '[name].bundle.js',

    // The filename of the SourceMaps for the JavaScript files.
    // They are inside the output.path directory.
    // See: http://webpack.github.io/docs/configuration.html#output-sourcemapfilename
    sourceMapFilename: '[name].map',

    // The filename of non-entry chunks as relative path
    // inside the output.path directory.
    // See: http://webpack.github.io/docs/configuration.html#output-chunkfilename
    chunkFilename: '[id].chunk.js'
  },

  // Add additional plugins to the compiler.
  //
  // See: http://webpack.github.io/docs/configuration.html#plugins
  plugins: [

    // Plugin: OccurenceOrderPlugin
    // Description: Varies the distribution of the ids to get the smallest id length
    // for often used ids.
    // See: https://webpack.github.io/docs/list-of-plugins.html#occurrenceorderplugin
    // See: https://github.com/webpack/docs/wiki/optimization#minimize
    new webpack.optimize.OccurenceOrderPlugin(true),

    // Plugin: CommonsChunkPlugin
    // Description: Shares common code between the pages.
    // It identifies common modules and put them into a commons chunk.
    // See: https://webpack.github.io/docs/list-of-plugins.html#commonschunkplugin
    // See: https://github.com/webpack/docs/wiki/optimization#multi-page-app
    new webpack.optimize.CommonsChunkPlugin({
      name: helpers.reverse(['main']),
      minChunks: Infinity
    }),

    // Plugin: HtmlWebpackPlugin
    // Description: Simplifies creation of HTML files to serve your webpack bundles.
    // This is especially useful for webpack bundles that include a hash in the filename
    // which changes every compilation.
    // See: https://github.com/ampedandwired/html-webpack-plugin
    new HtmlWebpackPlugin({
      template: 'src/pages/index.html',
      filename: '../index.html',
      chunksSortMode: 'none'
    }),

    // Plugin: CopyWebpackPlugin
    // Description: Copy files and directories in webpack.
    // See: https://www.npmjs.com/package/copy-webpack-plugin
    new CopyWebpackPlugin([
      {
        from: 'src/images',
        to: '../images'
      }
    ])
  ],

  // Include polyfills or mocks for various node stuff
  // Description: Node configuration
  // See: https://webpack.github.io/docs/configuration.html#node
  node: {
    global: 'window',
    crypto: 'empty',
    process: false,
    module: false,
    clearImmediate: false,
    setImmediate: false
  },

  // Don't swallow Webpack build failures
  // https://webpack.github.io/docs/configuration.html#bail
  bail: true
};
