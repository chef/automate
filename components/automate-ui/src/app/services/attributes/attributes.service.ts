import { Injectable } from '@angular/core';
import { NodeAttributes, RespNodeAttributes } from '../../types/types';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../environments/environment';
const CONFIG_MGMT_URL = environment.config_mgmt_url;

@Injectable()
export class AttributesService {

  constructor(private httpClient: HttpClient) {}

  nullNodeAttributes = new NodeAttributes({
    node_id: '',
    name: '',
    run_list: [],
    chef_environment: '',
    normal: '',
    normal_value_count: 0,
    default: '',
    default_value_count: 0,
    override: '',
    override_value_count: 0,
    automatic: '',
    automatic_value_count: 0,
    all_value_count: 0
  });

  fetch(nodeId: string): Promise<NodeAttributes> {
    const url = `${CONFIG_MGMT_URL}/nodes/${nodeId}/attribute`;

    return this.httpClient
      .get<RespNodeAttributes>(url).toPromise()
      .then((res) => new NodeAttributes(res))
      .catch( reason => {
        console.error(reason);
        return this.nullNodeAttributes;
      });
  }
}
