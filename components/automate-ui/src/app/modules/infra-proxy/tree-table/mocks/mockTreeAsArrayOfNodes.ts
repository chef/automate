import { Node } from '../models';
import { ChildList } from './models';

export const mockTreeAsArrayOfNodes: Node<ChildList>[] = [
  {
    value: {
      name: 'run_list1',
      version: '0.1',
      type: 'role'
    },
    children: [
      {
        value: {
          name: 'run_list2',
          version: '0.2',
          type: 'recipe'
        },
        children: []
      },
      {
        value: {
          name: 'run_list3',
          version: '0.2',
          type: 'recipe'
        },
        children: [
          {
            value: {
              name: 'run_list4',
              version: '0.2',
              type: 'recipe'
            },
            children: []
          }
        ]
      }
    ]
  },
  {
    value: {
      name: 'run_list12',
      version: '0.2',
      type: 'role'
    },
    children: [
      {
        value: {
          name: 'run_list122',
          version: '0.2',
          type: 'recipe'
        },
        children: []
      },
      {
        value: {
          name: 'run_list2222',
          version: '0.2',
          type: 'recipe'
        },
        children: []
      }
    ]
  }
];
