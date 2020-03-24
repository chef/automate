export interface Cookbook {
    name?: string;
    current_version?: string;
    versions?: string[];
    cookbook_name?: string;
    version?: string;
    chef_type?: string;
    frozen?: string;
    json_class?: string;
    files?: string[];
    templates?: string[];
    attributes?: string[];
    recipes?: string[];
    definitions?: string[];
    libraries?: string[];
    providers?: string[];
    resources?: string[];
    root_files?: string[];
    metadata?: string[];
    access?: string[];
}
