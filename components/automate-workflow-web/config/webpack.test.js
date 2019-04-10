var helpers = require('./helpers');
var CopyWebpackPlugin = require('copy-webpack-plugin');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var DefinePlugin = require('webpack/lib/DefinePlugin');

/**
 * Webpack configuration
 * See: http://webpack.github.io/docs/configuration.html#cli
 */
module.exports = {

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
        exclude: [/\.(e2e)\.ts$/]
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
    ],

    // An array of applied pre and post loaders.
    // See: http://webpack.github.io/docs/configuration.html#module-preloaders-module-postloaders
    postLoaders: [

      // Instruments JS files with Istanbul for subsequent code coverage reporting.
      // Instrument only testing sources.
      // See: https://github.com/deepsweet/istanbul-instrumenter-loader
      {
        test: /\.(js)$/, loader: 'istanbul-instrumenter-loader',
        include: helpers.root('src'),
        exclude: [
          /\.(e2e|spec)\.js$/,
          /node_modules/
        ]
      }
    ],

    // Add additional plugins to the compiler.
    // See: http://webpack.github.io/docs/configuration.html#plugins
    plugins: [

      // Plugin: HtmlWebpackPlugin
      // Description: Simplifies creation of HTML files to serve your webpack bundles.
      // This is especially useful for webpack bundles that include a hash in the filename
      // which changes every compilation.
      // See: https://github.com/ampedandwired/html-webpack-plugin
      new HtmlWebpackPlugin({
        template: 'src/pages/index.html',
        chunksSortMode: 'none'
      }),

      // Plugin: CopyWebpackPlugin
      // Description: Copy files and directories in webpack.
      // See: https://www.npmjs.com/package/copy-webpack-plugin
      new CopyWebpackPlugin([{
        from: 'src/images',
        to: 'images'
      }]),

		  new DefinePlugin({
    	  'A2URL': JSON.stringify('/a2/placeholder.html')
   		})
    ]
  }
};
