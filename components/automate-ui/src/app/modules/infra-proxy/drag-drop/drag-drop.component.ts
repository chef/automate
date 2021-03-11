import {
    Component,
    Input,
    OnDestroy,
    OnInit,
    Output,
    EventEmitter
} from '@angular/core';
import { Store } from '@ngrx/store';
import { isNil } from 'lodash/fp';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { EntityStatus } from 'app/entities/entities';
import { CdkDragDrop, moveItemInArray, transferArrayItem } from '@angular/cdk/drag-drop';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';
import { GetRecipes } from 'app/entities/recipes/recipe.action';
import {
    allRecipes,
    getAllStatus as getAllRecipesForOrgStatus
} from 'app/entities/recipes/recipe.selectors';
import { combineLatest, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { FormGroup, FormControl } from '@angular/forms';
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

    @Output() valueChanged: EventEmitter<ListItem[]> = new EventEmitter();
    private isDestroyed = new Subject<boolean>();
    public typeAvailable: string[] = ['available roles and recipes', 'available roles', 'available recipes'];
    public defaultType = 'available roles and recipes';
    availableType: AvailableType[] = [];
    drops = [];
    recipes: string[] = [];
    userForm: FormGroup;
    public showbutton = false;

    public selected: ListItem[] = [];
    list: string[] = [];

    constructor(
        private store: Store<NgrxStateAtom>
    ) {

    }

    ngOnInit() {
        this.store.dispatch(new GetRecipes({
            server_id: this.serverId, org_id: this.orgId, name: '_default'
        }));

        combineLatest([
            this.store.select(getAllRecipesForOrgStatus),
            this.store.select(allRecipes)
        ]).pipe(takeUntil(this.isDestroyed))
            .subscribe(([getRecipesSt, allRecipesState]) => {
                if (getRecipesSt === EntityStatus.loadingSuccess && !isNil(allRecipesState)) {
                    this.recipes = allRecipesState;
                    this.mergeArray(this.recipes, this.defaultType);
                }
            });

        this.userForm = new FormGroup({
            'firstNameControl': new FormControl(null),
            'selected': new FormControl({ value: [] }),
            'test': new FormControl(null)
        });

    }

    selectChangeHandler(id: string): void {
        this.defaultType = id;
        this.mergeArray(this.recipes, this.defaultType);

    }

    mergeArray(recipeList: string[], id: string) {
        this.showbutton = true;
        this.availableType = [];
        console.log(recipeList);
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
            console.log(this.availableType);

        } else if (id === 'available roles') {
            this.roles.forEach((role) => {
                this.availableType.push({
                    name: role.name,
                    type: 'role'
                });
            });
            console.log(this.availableType);

        } else {
            recipeList.forEach((recipe) => {
                this.availableType.push({
                    name: recipe,
                    type: 'recipe'
                });
            });
            console.log(this.availableType);

        }

        console.log(this.availableType);

    }
    selectedItemsHandler(value: ListItem[]) {
        this.selected = value;
        this.valueChanged.emit(this.selected);
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

    ngOnDestroy(): void {
        this.isDestroyed.next(true);
        this.isDestroyed.complete();
    }
}
