
export class ChefSorters {

    public static normalSort(input: object[]): object[] {
        const sortedObject = Object.values(input);

        const opts = {
            numeric: true,
            sensitivity: 'base'
        };

        // per @msorens https://github.com/chef/a2/pull/4434
        // Sort by name then by cased-name, since no other field is useful as a secondary sort;
        // this ensures stable sort with respect to case, so 'a' always comes before 'A'.
        sortedObject.sort(
            (a, b) =>
                a.name.localeCompare(b.name, undefined, opts) ||
                a.name.localeCompare(b.name, undefined, { numeric: true })
        );

        console.log("**********")
        console.log(sortedObject)

        return sortedObject;
    }
}
