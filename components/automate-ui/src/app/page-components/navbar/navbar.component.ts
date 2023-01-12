import { Component } from '@angular/core';
import { ProductDeployedService } from 'app/services/product-deployed/product-deployed.service';
// import { LoadRemoteModuleOptions } from '@angular-architects/module-federation';

@Component({
  selector: 'app-navbar',
  templateUrl: './navbar.component.html',
  styleUrls: ['./navbar.component.scss']
})



export class NavbarComponent {

  public isDesktopView = false;

  // plugins: PluginOptions[] = [];
  // workflow: PluginOptions[] = [];

//   lookup(): Promise<PluginOptions[]> {
//     return Promise.resolve([
//         {
//             type: 'module',
//             remoteEntry: 'http://localhost:4201/remoteEntry.js',
//             exposedModule: './Download',

//             displayName: 'Download',
//             componentName: 'DownloadComponent'
//         },
//         {
//             type: 'module',
//             remoteEntry: 'http://localhost:4201/remoteEntry.js',
//             exposedModule: './Upload',

//             displayName: 'Upload',
//             componentName: 'UploadComponent'
//         },
//         {
//             type: 'module',
//             remoteEntry: 'http://localhost:4202/remoteEntry.js',
//             exposedModule: './Analyze',

//             displayName: 'Analyze',
//             componentName: 'AnalyzeComponent'
//         },
//         {
//             type: 'module',
//             remoteEntry: 'http://localhost:4202/remoteEntry.js',
//             exposedModule: './Enrich',

//             displayName: 'Enrich',
//             componentName: 'EnrichComponent'
//         }
//     ] as PluginOptions[]);
// }
  constructor(private productDeployedService: ProductDeployedService) {
    this.isDesktopView = this.productDeployedService.isProductDeployed('desktop');
  }

  // async ngOnInit(): Promise<void> {
  //   this.plugins = await this.lookup();
  // }

  // add(plugin: PluginOptions): void {
  //   console.log('plugsin add @@@', plugin)
  //   this.workflow.push(plugin);
  // }



}
