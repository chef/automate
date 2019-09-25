
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

    public static naturalSort( input: Array<any>, property: Array<string> | string ): Array<any> {
        // per @msorens https://github.com/chef/a2/pull/4434
        // Stable sort so 'a' always comes before 'A'.
        const opts = { numeric: true, sensitivity: 'base' };

        // propComparator function idea from Dave Newton
        // https://stackoverflow.com/questions
        const propComparator = (propName) => {
            console.log(propName);
            return (a, b) => a[propName].localeCompare(b[propName], undefined, opts) ||
                      a[propName].localeCompare(b[propName], undefined, { numeric: true });
        };

        const dualPropComparator = (propOne, propTwo) => {
            console.log(`PropOne: ${propOne}`);
            console.log('PropTwo: ${propTwo}');
            return (a, b) => a[propOne].localeCompare(b[propOne], undefined, opts) ||
                      a[propOne].localeCompare(b[propOne], undefined, { numeric: true }) ||
                      a[propTwo].localeCompare(b[propTwo], undefined, opts);
        };

        // let sorted;
        // // if property argument is an array, use the dualPropComparator
        // if ( property instanceof Array ) {
        //     console.log(typeof property);
        //     sorted =  input.sort( dualPropComparator(property[0], property[1]) );
        // } else {
        //     console.log(typeof property);
        //     sorted = input.sort( propComparator(property) );
        // }
        return property instanceof Array
            ? input.sort(dualPropComparator(property[0], property[1]))
            : input.sort(propComparator(property));

    }
}
