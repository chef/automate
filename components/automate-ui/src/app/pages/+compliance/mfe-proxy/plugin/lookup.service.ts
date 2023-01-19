import { Injectable } from '@angular/core';
import { PluginOptions } from './plugin';

@Injectable({ providedIn: 'root' })

export class LookupService {
    lookup(): Promise<PluginOptions[]> {
        return Promise.resolve([
            {
                type: "module",
                remoteEntry: 'http://localhost:4209/remoteEntry.js',
                exposedModule: './homeComponent',

                displayName: 'HomeComponent',
                componentName: 'HomeComponent'
            }
        ] as PluginOptions[]);
    }
}
