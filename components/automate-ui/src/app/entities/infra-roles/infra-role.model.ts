export interface InfraRole {
    name: string;
}

export interface RoleItem {
    name: string;
    chef_type:string;
    description: string;
    run_list: string[];
}