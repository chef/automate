// const ModuleFederationPlugin = require("webpack/lib/container/ModuleFederationPlugin");
// const mf = require("@angular-architects/module-federation/webpack");
// const path = require("path");
// const { withModuleFederationPlugin } = require("@angular-architects/module-federation/webpack");
// const share = mf.share;

// const sharedMappings = new mf.SharedMappings();
// sharedMappings.register(
//   path.join(__dirname, 'tsconfig.json'),
//   [
//     '@shared'
//   ]);

// module.exports = {
//   output: {
//     uniqueName: "shell",
//     publicPath: "auto"
//   },
//   optimization: {
//     runtimeChunk: false
//   },   
//   resolve: {
//     alias: {
//       ...sharedMappings.getAliases(),
//     }
//   },
//   experiments: {
//     outputModule: true
//   },
//   plugins: [
//     new ModuleFederationPlugin({
//       name: "automate",
//       remoteType: 'var',
//       remotes: {
//         compliance: 'compliance',
//       },  
//       library: { type: "module" },
        
//         shared: share({
//           "@angular/core": { singleton: true, strictVersion: true, requiredVersion: 'auto' }, 
//           "@angular/common": { singleton: true, strictVersion: true, requiredVersion: 'auto' }, 
//           "@angular/common/http": { singleton: true, strictVersion: true, requiredVersion: 'auto' }, 
//           "@angular/router": { singleton: true, strictVersion: true, requiredVersion: 'auto' },

//           ...sharedMappings.getDescriptors()
//         })
        
//     }),
//     // withModuleFederationPlugin({
//     //   remote: {
//     //     compliance:  'http://localhost:4209/remoteEntry.js'
//     //   },
//     //   shared: share({
//     //           "@angular/core": { singleton: true, strictVersion: true, requiredVersion: 'auto' }, 
//     //           "@angular/common": { singleton: true, strictVersion: true, requiredVersion: 'auto' }, 
//     //           "@angular/common/http": { singleton: true, strictVersion: true, requiredVersion: 'auto' }, 
//     //           "@angular/router": { singleton: true, strictVersion: true, requiredVersion: 'auto' },
    
//     //           ...sharedMappings.getDescriptors()
//     //         })
//     // }),
//     sharedMappings.getPlugin()
//   ],
// };


// const ModuleFederationPlugin = require("webpack/lib/container/ModuleFederationPlugin");
// const mf = require("@angular-architects/module-federation/webpack");
// const path = require("path");
// const share = mf.share;

// const sharedMappings = new mf.SharedMappings();
// sharedMappings.register(
//   path.join(__dirname, 'tsconfig.app.json'),
//   [/* mapped paths to share */]);

// module.exports = {
//   output: {
//     uniqueName: "shell",
//     publicPath: "auto",
//     scriptType: 'text/javascript'
//   },
//   optimization: {
//     runtimeChunk: false
//   },
//   resolve: {
//     alias: {
//       ...sharedMappings.getAliases(),
//     }
//   },
//   experiments: {
//     outputModule: true
//   },
//   plugins: [
//     new ModuleFederationPlugin({
//       name: 'host',
//       remotes: {
//         app1: `promise new Promise(resolve => {
//       const urlParams = new URLSearchParams(window.location.search)
//       // const version = urlParams.get('app1VersionParam')
//       // This part depends on how you plan on hosting and versioning your federated modules
//       const remoteUrlWithVersion = 'http://localhost:4209/remoteEntry.js'
//       const script = document.createElement('script')
//       script.src = remoteUrlWithVersion
//       script.onload = () => {
//         // the injected script has loaded and is available on window
//         // we can now resolve this Promise
//         const proxy = {
//           get: (request) => window.app1.get(request),
//           init: (arg) => {
//             try {
//               return window.app1.init(arg)
//             } catch(e) {
//               console.log('remote container already initialized')
//             }
//           }
//         }
//         resolve(proxy)
//       }
//       // inject this script with the src set to the versioned remoteEntry.js
//       document.head.appendChild(script);
//     })
//     `,
//       },
//       // ...
//     }),
//     sharedMappings.getPlugin()
//   ],
// };

const { shareAll, withModuleFederationPlugin } = require('@angular-architects/module-federation/webpack');

module.exports = withModuleFederationPlugin({

  remotes: {
    // "mfe1": "http://localhost:4201/remoteEntry.js",    
  },

  shared: {
    ...shareAll({ singleton: true, strictVersion: true, requiredVersion: 'auto' }),
  },

});
