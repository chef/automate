import { Menu } from './../../entities/cookbooks/cookbook-details.model';

export class CollapsibleListMapper {
    public static transform(resp, keys): Menu[] {
        return keys.map(key => {
            return {
                menu: key,
                subMenu: resp[key].map(data => {
                    return { name: data.name, url: data.url };
                })
            };
        });

    }
}
