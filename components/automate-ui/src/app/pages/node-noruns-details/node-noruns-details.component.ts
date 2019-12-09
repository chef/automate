import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import {
  Node
} from '../../entities/client-runs/client-runs.model';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';

@Component({
  selector: 'app-node-noruns-details',
  templateUrl: './node-noruns-details.component.html',
  styleUrls: ['./node-noruns-details.component.scss']
})
export class NodeNoRunsDetailsComponent implements OnInit {
  nodeId: string;
  node: Node;

  constructor(
    private route: ActivatedRoute,
    private layoutFacade: LayoutFacadeService
  ) {}

  ngOnInit() {
    this.layoutFacade.showSidebar('infrastructure');
    this.nodeId = this.getRouteParam('node-id');
    this.route.data.subscribe((data: { node: Node }) => {
        this.node = data.node;
    });
  }

  private getRouteParam(paramName: string): string | undefined {
    return this.route.snapshot.params[paramName];
  }
}
