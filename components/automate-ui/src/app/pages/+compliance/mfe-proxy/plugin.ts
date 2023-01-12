import { LoadRemoteModuleOptions } from '@angular-architects/module-federation';

export type PluginOptions = LoadRemoteModuleOptions & {
    displayName: string;
    componentName: string;
};
