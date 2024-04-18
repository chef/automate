import { Entity } from '../entities';
import {
  filterTags,
  findTag,
  getComponentMetaData,
  getComponentProps,
  getType
} from './doc.helpers';

interface Property {
  name: string;
  description?: string;
  defaultValue?: string;
}

export class DocEntity implements Entity {
  id: string;
  name: string;
  description: string;
  tag: string;
  styleUrl: string;
  shadow: boolean;
  properties: Property[];
  examples: string[];
  docType: string;

  constructor(jsonDoc:any = {}) {
    const entity = jsonDoc?.children[0];
    const { tag, styleUrl, shadow } = getComponentMetaData(entity);

    this.id = tag;
    this.name = entity['name'];
    this.description = findTag('description', entity);
    this.tag = tag;
    this.styleUrl = styleUrl || '';
    this.shadow = shadow || false;
    this.properties = getComponentProps(entity['children']);
    this.examples = filterTags('@example', entity);
    this.docType = getType(entity);
  }

  static create(json) {
    return new DocEntity(json);
  }
}
