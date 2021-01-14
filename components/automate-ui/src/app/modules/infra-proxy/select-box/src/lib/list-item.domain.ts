export class ListItem {
    constructor(value: string) {
        this.value = value;
        this.selected = false;
    }
    public value: string;
    public selected: boolean = false;

}