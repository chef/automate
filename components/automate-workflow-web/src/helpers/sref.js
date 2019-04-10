import { map } from 'lodash';

// accepts a string (statename) and an object (params)
// returns a string suitable for use with ui-router's ui-sref directive.
export default function sref(name, params) {
  let stringParams = map(params, (value, key) => `${key}:'${value}'`).join(',');
  return `${name}({${stringParams}})`;
}
