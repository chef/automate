
export class ChefSorters {

    // normalSort takes in an Object and returns an Array of Objects
    // sorted by the property chosen.

    public static normalSort(input: object, property: string): Array<any> {
        const sortedArray = Object.values(input);

        // options for localeCompare
        const opts = {
            numeric: true,
            sensitivity: 'base'
        };

        // per @msorens https://github.com/chef/a2/pull/4434
        // Stable sort so 'a' always comes before 'A'.

        // propComparator function idea from Dave Newton
        // https://stackoverflow.com/questions
        const propComparator = (propName) =>
            (a, b) => a[propName].localeCompare(b[propName], undefined, opts) ||
                      a[propName].localeCompare(b.name, undefined, { numeric: true });

        sortedArray.sort(propComparator(property));

        return sortedArray;
    }
}
