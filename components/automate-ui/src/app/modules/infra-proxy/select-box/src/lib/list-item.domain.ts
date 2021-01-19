export class ListItem {
    constructor(value: string, type: string) {
        this.value = value;
        this.selected = false;
        this.type = type;
    }
    public value: string;
    public selected: boolean = false;
    public type: string;
}
