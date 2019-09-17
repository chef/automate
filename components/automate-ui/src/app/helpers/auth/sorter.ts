
export class ChefSorters {

    public static normalSort(input: object, property: string): Array<any> {
        const sortedObject = Object.values(input);

        const opts = {
            numeric: true,
            sensitivity: 'base'
        };

        // per @msorens https://github.com/chef/a2/pull/4434
        // Sort by name then by cased-name, since no other field is useful as a secondary sort;
        // this ensures stable sort with respect to case, so 'a' always comes before 'A'.

        // propComparator function idea from Dave Newton
        // https://stackoverflow.com/questions
        const propComparator = (propName) =>
            (a, b) => a[propName].localeCompare(b[propName], undefined, opts) ||
                      a[propName].localeCompare(b.name, undefined, { numeric: true });

        sortedObject.sort(propComparator(property));

        return sortedObject;
    }
}
