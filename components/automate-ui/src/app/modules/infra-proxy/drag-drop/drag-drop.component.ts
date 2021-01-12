import {
  Component,
  Input,
  OnDestroy,
  OnInit
} from '@angular/core';
import { Store } from '@ngrx/store';
import { isNil } from 'lodash/fp';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { EntityStatus } from 'app/entities/entities';
import {CdkDragDrop, moveItemInArray, transferArrayItem} from '@angular/cdk/drag-drop';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';
import { GetRecipes } from 'app/entities/recipes/recipe.action';
import {
  allRecipes,
  getAllStatus as getAllRecipesForOrgStatus
} from 'app/entities/recipes/recipe.selectors';
import { combineLatest, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';


@Component({
  selector: 'app-drag-drop',
  templateUrl: './drag-drop.component.html',
  styleUrls: ['./drag-drop.component.scss']
})
export class DragDropComponent implements OnInit, OnDestroy {
  @Input() roles: InfraRole[] = [];
  @Input() serverId: string;
  @Input() orgId: string;

  private isDestroyed = new Subject<boolean>();
  public typeAvailable: string[] = ['available roles and recipes', 'available roles', 'available recipes']
  public defaultType = 'available roles and recipes'
  
  drops = [];
  recipes: string[] = [];

  constructor(
    private store: Store<NgrxStateAtom>,
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
    .subscribe(([ getRecipesSt, allRecipesState]) => {
      if (getRecipesSt === EntityStatus.loadingSuccess && !isNil(allRecipesState)) {
        this.recipes = allRecipesState;
      }
    });
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

