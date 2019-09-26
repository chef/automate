export class ChefSorters {
    // ***********
    // naturalSort takes in an Array of Objects and returns an Array of Objects
    // sorted 'naturally' by the property chosen.
    // @input: Array of Objects
    // @property: string or an Array of strings, max length is 2 properties
    //            when using an Array, will sort first by string one, then by string 2;
    //
    // example use with single prop:  ChefSorters.naturalSort(users, 'name');
    // example use with dual prop:    ChefSorters.naturalSort(users, ['name', 'id']);
    // ***********

    // tslint:disable-next-line: max-line-length
    public static naturalSort(input: Array<any>, property: Array<string> | string): Array<any> {
        // per @msorens https://github.com/chef/a2/pull/4434
        // Stable sort so 'a' always comes before 'A'.
        const naturalAndCaseInsensitive = { numeric: true, sensitivity: 'base' };
        const naturalAndCaseSensitive = { numeric: true };

        // propComparator function idea from Dave Newton
        // https://stackoverflow.com/questions/8537602
        const propComparator = (propName) => {
            return (a, b) => a[propName].localeCompare(b[propName],
                                                       undefined, naturalAndCaseInsensitive) ||
                a[propName].localeCompare(b[propName], undefined, naturalAndCaseSensitive);
        };

        const dualPropComparator = (propOne, propTwo) => {
            return (a, b) => a[propOne].localeCompare(b[propOne],
                                                     undefined, naturalAndCaseInsensitive) ||
                a[propOne].localeCompare(b[propOne], undefined, naturalAndCaseSensitive) ||
                a[propTwo].localeCompare(b[propTwo], undefined, naturalAndCaseInsensitive);
        };

        return property instanceof Array
            ? input.sort(dualPropComparator(property[0], property[1]))
            : input.sort(propComparator(property));
    }
}
