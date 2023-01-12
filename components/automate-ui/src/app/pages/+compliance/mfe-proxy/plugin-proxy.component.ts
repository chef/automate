// import { Component, Input, OnChanges, ViewChild, ViewContainerRef } from '@angular/core';
// import { loadRemoteModule } from '@angular-architects/module-federation';
// import { LoadRemoteModuleOptions } from '@angular-architects/module-federation';


// export type PluginOptions = LoadRemoteModuleOptions & {
//     displayName: string;
//     componentName: string;
// };


// @Component({
//     standalone: true,
//     selector: 'plugin-proxy',
//     template: `
//         <ng-container #placeHolder></ng-container>
//     `
// })
// export class PluginProxyComponent implements OnChanges {
//     @ViewChild('placeHolder', { read: ViewContainerRef, static: true })
//     viewContainer: ViewContainerRef;

//     constructor() { }

//     // @Input() options: PluginOptions;
//     _options: any

//     @Input() set options(p) {
//         debugger
//         this._options = p;
//     }

//     async ngOnChanges() {
//         this.viewContainer.clear();

//         const Component = await loadRemoteModule(this._options)
//             .then(m => m[this._options.componentName]);

//         this.viewContainer.createComponent(Component);
//     }
// }


import { Component, Input, OnChanges, ViewChild, ViewContainerRef } from '@angular/core';
import { loadRemoteModule } from '@angular-architects/module-federation';
import { PluginOptions } from './plugin';

@Component({
    standalone: true,
    selector: 'plugin-proxy',
    template: `
        <ng-container #placeHolder></ng-container>
    `
})
export class PluginProxyComponent implements OnChanges {
    @ViewChild('placeHolder', { read: ViewContainerRef, static: true })
    viewContainer: ViewContainerRef;

    constructor() { }

    @Input() options: PluginOptions;

    async ngOnChanges() {
        this.viewContainer.clear();

        const Component = await loadRemoteModule(this.options)
            .then(m =>{
                console.warn(m[this.options.componentName],'check@@@@')
                return m[this.options.componentName]});

        this.viewContainer.createComponent(Component);
    }
}

