export interface RespCookbookDetails {
    name: string;
    cookbook_name: string;
    version: string;
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
    root_files?: RootFiles[];
    metadata?: string[];
    access?: string[];
}

export interface CookbookDetails {
    cookbook_name: string;
    name_and_version: string;
    version: string;
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
    root_files?: RootFiles[];
    metadata?: string[];
    access?: string[];
}

export interface RootFiles {
    url: string;
    path: string;
    name: string;
    checksum: string;
    specificity: string;
}

export interface SubMenu {
    name: string;
    url: string;
}

export interface Menu {
    menu: string;
    subMenu: SubMenu[];
}
