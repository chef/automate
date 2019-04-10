import {
  ChangeDetectionStrategy,
  ChangeDetectorRef,
  Component,
  Input,
  OnInit,
  ViewChild
} from '@angular/core';
import { NodeAttributes } from '../../types/types';
import { AttributesService } from '../../services/attributes/attributes.service';
import { JsonTreeComponent } from '../json-tree/json-tree.component';

@Component({
  selector: 'app-attributes',
  templateUrl: './attributes.component.html',
  styleUrls: ['./attributes.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class AttributesComponent implements OnInit {
  @Input() nodeId: string;

  @ViewChild(JsonTreeComponent)
  tree: JsonTreeComponent;

  selected_level = 'all';
  attributes: NodeAttributes = this.attributesService.nullNodeAttributes;
  selectedAttrs: any;
  searchTerm: string;
  resultCount: number;
  resultSummary: string;

  // precedence levels
  override = 'override';
  default = 'default';
  normal = 'normal';
  automatic = 'automatic';
  all = 'all';

  constructor(
    private attributesService: AttributesService,
    private changeDetectorRef: ChangeDetectorRef
  ) {}

  ngOnInit() {
    this.attributesService.fetch(this.nodeId).then(data => {
        this.attributes = data;
        this.changeDetectorRef.markForCheck();

        // Don't block the UI from displaying attribute counts
        // while the tree initializes
        setTimeout(() => this.filter(this.selected_level), 10);
      }, error => {
        console.error('Error: ' + error);
      });
  }

  // set selected item to selected_level
  filter(precedence_level: string): void {
    this.resetSearch();
    this.selected_level = precedence_level;
    this.selectedAttrs = this.retrieve(precedence_level);
    this.changeDetectorRef.markForCheck();
  }

  // retrieve attributes based on their level of precedence
  retrieve(level: string): any {
    switch (level) {
      case this.all: {
        return this.attributes.all;
      }
      case this.default: {
        return this.attributes.default;
      }
      case this.normal: {
        return this.attributes.normal;
      }
      case this.override: {
        return this.attributes.override;
      }
      case this.automatic: {
        return this.attributes.automatic;
      }
      default: {
        return {};
      }
    }
  }

  search() {
    this.resultCount = this.tree.search(this.searchTerm);

    if (this.resultCount === 1) {
      this.resultSummary = '1 result';
    } else {
      this.resultSummary = `${this.resultCount || 'No'} results`;
    }
  }

  resetSearch() {
    this.searchTerm = this.resultCount = this.resultSummary = null;
    this.tree.reset();
  }
}
