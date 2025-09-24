import { Component } from '@angular/core';
import { ProjectsFilterService } from '../../services/projects-filter/projects-filter.service';

@Component({
  standalone: false,
  selector: 'app-projects-filter',
  templateUrl: './projects-filter.component.html',
  styleUrls: [ './projects-filter.component.scss' ]
})
export class ProjectsFilterComponent {
  constructor(public projectsFilter: ProjectsFilterService) {
    this.projectsFilter.loadOptions();
  }
}
