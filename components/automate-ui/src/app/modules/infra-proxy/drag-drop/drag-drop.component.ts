import {
    Component,
    Input,
    OnDestroy,
    OnInit,
    Output,
    EventEmitter
} from '@angular/core';
import { Subject } from 'rxjs';
import { FormGroup, FormControl } from '@angular/forms';
import { CdkDragDrop, moveItemInArray, transferArrayItem } from '@angular/cdk/drag-drop';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';
import { ListItem } from '../select-box/src/lib/list-item.domain';

export interface AvailableType {
    name: string;
    type: 'role' | 'recipe';
}

@Component({
    selector: 'app-drag-drop',
    templateUrl: './drag-drop.component.html',
    styleUrls: ['./drag-drop.component.scss']
})
export class DragDropComponent implements OnInit, OnDestroy {
    @Input() roles: InfraRole[] = [];
    @Input() serverId: string;
    @Input() orgId: string;
    @Input() selected: ListItem[] = [];
    @Input() recipes: string[] = [];

    @Output() valueChanged: EventEmitter<ListItem[]> = new EventEmitter();

    public availableType: AvailableType[] = [];
    public defaultType = 'available roles and recipes';
    public selectedItems: ListItem[] = [];
    public showbutton = false;
    public typeAvailable: string[] = ['available roles and recipes', 'available roles', 'available recipes'];
    public userForm: FormGroup;

    private isDestroyed = new Subject<boolean>();

    constructor(
    ) {
    }

    ngOnInit() {
        this.mergeArray(this.recipes, this.defaultType);

        this.userForm = new FormGroup({
            'firstNameControl': new FormControl(null),
            'selected': new FormControl({ value: [] }),
            'test': new FormControl(null)
        });
    }

    ngOnDestroy(): void {
        this.isDestroyed.next(true);
        this.isDestroyed.complete();
    }

    drop(event: CdkDragDrop<string[]>) {
        if (event.previousContainer === event.container) {
            moveItemInArray(event.container.data, event.previousIndex, event.currentIndex);
        } else {
            transferArrayItem(event.previousContainer.data,
                event.container.data,
                event.previousIndex,
                event.currentIndex);
        }
    }

    selectChangeHandler(id: string): void {
        this.defaultType = id;
        this.mergeArray(this.recipes, this.defaultType);
    }

    selectedItemsHandler(value: ListItem[]) {
        this.selected = value;
        this.valueChanged.emit(this.selected);
    }

    private mergeArray(recipeList: string[], id: string) {
        this.showbutton = true;
        this.availableType = [];

        if (id === 'available roles and recipes') {
            if (this.recipes.length > 0) {
                this.roles.forEach((role) => {
                    this.availableType.push({
                        name: role.name,
                        type: 'role'
                    });
                });
                recipeList.forEach((recipe) => {
                    this.availableType.push({
                        name: recipe,
                        type: 'recipe'
                    });
                });
            }
        } else if (id === 'available roles') {
            this.roles.forEach((role) => {
                this.availableType.push({
                    name: role.name,
                    type: 'role'
                });
            });
        } else {
            recipeList.forEach((recipe) => {
                this.availableType.push({
                    name: recipe,
                    type: 'recipe'
                });
            });
        }
    }

}
