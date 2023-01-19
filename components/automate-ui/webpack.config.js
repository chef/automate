const { shareAll, withModuleFederationPlugin } = require('@angular-architects/module-federation/webpack');

module.exports = withModuleFederationPlugin({

  // remotes: {
  //   "compliance": "http://localhost:4209/remoteEntry.js",
  // },

  // shared: {
  //   ...shareAll({ 
  //     singleton: true,
  //     eager: true,
  //     pinned: true,
  //     strictVersion: true,
  //     requiredVersion: 'auto'}),
  // },
  shared: {
    // ...shareAll({ singleton: true, strictVersion: true, requiredVersion: 'auto' }),
    "@angular/core": { singleton: true, eager: true, strictVersion: false, requiredVersion: '^14.0.0' },
    "@angular/common": { singleton: true, eager: true, strictVersion: false, requiredVersion: '^14.0.0' },
    "@angular/router": { singleton: true, eager: true, strictVersion: false, requiredVersion: '^14.0.0' },
    "@angular/animations": { singleton: true, eager: true, strictVersion: false, requiredVersion: '^14.0.0' }
  },

});