import { Node } from '../models';
import { ChildLists } from './models';

export const mockTree: Node<ChildLists> = {
  value: {
    name: 'run_list1',
    version: '0.1',
    type: 'role'
  },
  children: [
    {
      value: {
        name: 'run_list12',
        version: '0.1',
        type: 'recipe'
      },
      children: []
    },
    {
      value: {
        name: 'run_list13',
        version: '0.1',
        type: 'recipe'
      },
      children: []
    },
    {
      value: {
        name: 'run_list14',
        version: '0.1',
        type: 'recipe'
      },
      children: [
        {
          value: {
            name: 'run_list15',
            version: '0.1',
            type: 'recipe'
          },
          children: []
        },
        {
          value: {
            name: 'run_list16',
            version: '0.1',
            type: 'recipe'
          },
          children: [
            {
              value: {
                name: 'run_list17',
                version: '0.1',
                type: 'recipe'
              },
              children: []
            },
            {
              value: {
                name: 'run_list18',
                version: '0.1',
                type: 'recipe'
              },
              children: []
            }
          ]
        }
      ]
    }
  ]
};

