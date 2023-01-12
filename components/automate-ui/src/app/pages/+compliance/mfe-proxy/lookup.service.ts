import { Injectable } from '@angular/core';
import { PluginOptions } from './plugin';

@Injectable({ providedIn: 'root' })

export class LookupService {
    lookup(): Promise<PluginOptions[]> {
        return Promise.resolve([
            {
                type: 'module',
                remoteEntry: 'http://localhost:4209/remoteEntry.js',
                exposedModule: './homeComponent',

                displayName: 'HomeComponent',
                componentName: 'homeComponent'
            },
            {
                type: 'module',
                remoteEntry: 'http://localhost:4201/remoteEntry.js',
                exposedModule: './Upload',

                displayName: 'Upload',
                componentName: 'UploadComponent'
            },
            {
                type: 'module',
                remoteEntry: 'http://localhost:4202/remoteEntry.js',
                exposedModule: './Analyze',

                displayName: 'Analyze',
                componentName: 'AnalyzeComponent'
            },
            {
                type: 'module',
                remoteEntry: 'http://localhost:4202/remoteEntry.js',
                exposedModule: './Enrich',

                displayName: 'Enrich',
                componentName: 'EnrichComponent'
            }
        ] as PluginOptions[]);
    }
}
