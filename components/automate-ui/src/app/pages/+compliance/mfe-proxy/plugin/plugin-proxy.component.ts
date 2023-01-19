
import { Component, Input, ViewChild, ViewContainerRef, OnChanges } from '@angular/core';

import { loadRemoteModule } from '@angular-architects/module-federation';
import { PluginOptions } from './plugin';

@Component({
    selector: 'plugin-proxy',
    template: `
        <ng-container #placeHolder></ng-container>
    `
})

export class PluginProxyComponent implements OnChanges {

    @ViewChild('placeHolder', { read: ViewContainerRef, static: true })
    viewContainer: ViewContainerRef;

    constructor() { }

    _options: PluginOptions;
    @Input() set pluginData(opt) {
        this._options = opt;
    }

    async ngOnChanges() {
        this.viewContainer.clear();
        const Component = await loadRemoteModule(this._options)
            .then(m => {
                return m[this._options.componentName]
            });
        this.viewContainer.createComponent(Component);

    }
}
