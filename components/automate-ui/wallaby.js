// This "config" file comes from https://github.com/wallabyjs/ngCliWebpackSample/ .
// There are only a couple tweaks needed for Automate-UI, noted inline.

var wallabyWebpack = require('wallaby-webpack');
var path = require('path');

var compilerOptions = Object.assign(
  require('./tsconfig.json').compilerOptions,
  require('./src/tsconfig.spec.json').compilerOptions);

compilerOptions.module = 'CommonJs';

module.exports = function (wallaby) {

  var webpackPostprocessor = wallabyWebpack({
    entryPatterns: [
      'src/wallabyTest.js',
      'src/**/*spec.js'
    ],

    module: {
      rules: [
        {test: /\.css$/, loader: ['raw-loader']},
        {test: /\.html$/, loader: 'raw-loader'},
        {test: /\.ts$/, loader: '@ngtools/webpack', include: /node_modules/, query: {tsConfigPath: 'tsconfig.json'}},
        {test: /\.js$/, loader: 'angular2-template-loader', exclude: /node_modules/},
        {test: /\.styl$/, loaders: ['raw-loader', 'stylus-loader']},
        {test: /\.less$/, loaders: ['raw-loader', {loader: 'less-loader', options: {paths: [__dirname]}}]},
        {test: /\.scss$|\.sass$/, loaders: ['raw-loader', 'sass-loader']},
        {test: /\.(jpg|png|svg)$/, loader: 'url-loader?limit=128000'}
      ]
    },

    resolve: {
      extensions: ['.js', '.ts'],
      modules: [
        path.join(wallaby.projectCacheDir, 'src/app'),
        path.join(wallaby.projectCacheDir, 'src'),
        'node_modules'
      ]
    },
    node: {
      fs: 'empty',
      net: 'empty',
      tls: 'empty',
      dns: 'empty'
    }
  });

  return {
    files: [
      {pattern: 'src/**/*.+(ts|css|less|scss|sass|styl|html|json|svg)', load: false},
      // 2017.09.30 Automate-UI: Added json-tree item to this list
      {pattern: 'src/app/page-components/json-tree/vendor/**/*.*', load: false, instrument: false},
      {pattern: 'src/**/*.d.ts', ignore: true},
      {pattern: 'src/**/*spec.ts', ignore: true}
    ],

    tests: [
      {pattern: 'src/**/*spec.ts', load: false},
      {pattern: 'src/**/*e2e-spec.ts', ignore: true}
    ],

    testFramework: 'jasmine',

    compilers: {
      '**/*.ts': wallaby.compilers.typeScript(compilerOptions)
    },

    middleware: function (app, express) {
      var path = require('path');
      app.use('/favicon.ico', express.static(path.join(__dirname, 'src/favicon.ico')));
      app.use('/assets', express.static(path.join(__dirname, 'src/assets')));
    },

    env: {
      // 2017.09.30 Automate-UI: Set the test-runner to use chrome headless runner
      // instead of the default electron, since "ng test" also uses chrome headless.
      // Note that it is specified as just "chrome" here unlike in karma.conf.js,
      // and that wallaby passes in a --headless flag by itself.
      kind: 'chrome'
    },

    postprocessor: webpackPostprocessor,

    setup: function () {
      window.__moduleBundler.loadTests();
    },

    // 2019.01.07 Automate-UI: turn down verbosity in Wallaby.js Console
    debug: false
  };
};
