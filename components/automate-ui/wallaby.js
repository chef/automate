// This "config" file comes from https://github.com/wallabyjs/ngCliWebpackSample/

module.exports = function (wallaby) {
  const wallabyWebpack = require('wallaby-webpack');
  const path = require('path');
  const fs = require('fs');

  const specPattern = '/**/*spec.ts';
  const angularConfig = require('./angular.json');

  const projects = Object.keys(angularConfig.projects).map(key => {
    return { name: key, ...angularConfig.projects[key] };
  }).filter(project => project.sourceRoot);

  const applications = projects.filter(project => project.projectType === 'application');
  const libraries = projects.filter(project => project.projectType === 'library');

  const tsConfigFile = projects
    .map(project => path.join(__dirname, project.root, 'tsconfig.spec.json'))
    .find(tsConfig => fs.existsSync(tsConfig));

  const tsConfigSpec = tsConfigFile ? JSON.parse(fs.readFileSync(tsConfigFile)) : {};

  const compilerOptions = Object.assign(require('./tsconfig.json').compilerOptions, tsConfigSpec.compilerOptions);

  return {
    files: [
      { pattern: path.basename(__filename), load: false, instrument: false },
      ...projects.map(project => ({
        pattern: project.sourceRoot + '/**/*.+(ts|js|css|less|scss|sass|styl|html|json|svg)',
        load: false
      })),
      ...projects.map(project => ({
        pattern: project.sourceRoot + specPattern,
        ignore: true
      })),
      // 2017.09.30 Automate-UI: Added json-tree item to this list
      { pattern: 'src/app/page-components/json-tree/vendor/**/*.*', load: false, instrument: false },
      { pattern: './**/*.d.ts', ignore: true }
    ],

    tests: [
      ...projects.map(project => ({
        pattern: project.sourceRoot + specPattern,
        load: false
      }))
    ],

    testFramework: 'jasmine',

    compilers: {
      '**/*.ts': wallaby.compilers.typeScript({
        ...compilerOptions,
        getCustomTransformers: program => {
          return {
            before: [
              require('@ngtools/webpack/src/transformers/replace_resources').replaceResources(
                path => true,
                () => program.getTypeChecker(),
                false
              )
            ]
          };
        }
      })
    },

    preprocessors: {
      /* Initialize Test Environment for Wallaby */
      [path.basename(__filename)]: file => `
 import '@angular-devkit/build-angular/src/angular-cli-files/models/jit-polyfills';
 import 'zone.js/dist/zone-testing';
 import { getTestBed } from '@angular/core/testing';
 import { BrowserDynamicTestingModule,  platformBrowserDynamicTesting} from '@angular/platform-browser-dynamic/testing';

 getTestBed().initTestEnvironment(BrowserDynamicTestingModule, platformBrowserDynamicTesting());`
    },

    middleware: function (app, express) {
      const path = require('path');

      applications.forEach(application => {
        if (
          !application.architect ||
          !application.architect.test ||
          !application.architect.test.options ||
          !application.architect.test.options.assets
        ) {
          return;
        }

        application.architect.test.options.assets.forEach(asset => {
          app.use(asset.slice(application.sourceRoot.length), express.static(path.join(__dirname, asset)));
        });
      });
    },

    env: {
      kind: 'chrome'
    },

    postprocessor: wallabyWebpack({
      entryPatterns: [
        ...applications
          .map(project => project.sourceRoot + '/polyfills.js')
          .filter(polyfills => fs.existsSync(path.join(__dirname, polyfills.replace(/js$/, 'ts')))),
        path.basename(__filename),
        ...projects.map(project => project.sourceRoot + specPattern.replace(/ts$/, 'js'))
      ],

      module: {
        rules: [
          { test: /\.css$/, loader: ['raw-loader'] },
          { test: /\.html$/, loader: 'raw-loader' },
          {
            test: /\.ts$/,
            loader: '@ngtools/webpack',
            include: /node_modules/,
            query: { tsConfigPath: 'tsconfig.json' }
          },
          { test: /\.styl$/, loaders: ['raw-loader', 'stylus-loader'] },
          { test: /\.less$/, loaders: ['raw-loader', { loader: 'less-loader' }] },
          {
            test: /\.scss$|\.sass$/,
            loaders: [{ loader: 'raw-loader' }, { loader: 'sass-loader', options: { implementation: require('sass') } }]
          },
          { test: /\.(jpg|png|svg)$/, loader: 'raw-loader' }
        ]
      },

      resolve: {
        extensions: ['.js', '.ts'],
        modules: [
          __dirname,
          'node_modules',
          ...(projects.length ? projects.filter(project => project.root).map(project => project.root) : []),
          ...(projects.length ? projects.filter(project => project.sourceRoot).map(project => project.sourceRoot) : [])
        ],
        alias: libraries.reduce((result, project) => {
          result[project.name] = path.join(wallaby.projectCacheDir, project.sourceRoot, 'public-api');
          return result;
        }, {})
      }
    }),

    setup: function () {
      window.__moduleBundler.loadTests();
    }
  };
};
