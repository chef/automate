
export class ChefSorters {

    public static naturalSort(input: Array<any>, property: string): Array<any> {
        // per @msorens https://github.com/chef/a2/pull/4434
        // Stable sort so 'a' always comes before 'A'.

        // propComparator function idea from Dave Newton
        // https://stackoverflow.com/questions
        const propComparator = (propName) =>
            (a, b) => a[propName].localeCompare(b[propName], undefined, {
                numeric: true,
                sensitivity: 'base'
            }) ||
                a[propName].localeCompare(b[propName], undefined, { numeric: true });

        const sortedArray = input.sort(propComparator(property));

        return sortedArray;
    }
}
