
export function uuidv4() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
    // tslint:disable-next-line:no-bitwise
    const r = Math.random() * 16 | 0;
    // tslint:disable-next-line:no-bitwise
    const v = c === 'x' ? r : (r & 0x3 | 0x8);
    return v.toString(16);
  });
}

export function eventExist(entityName: string, events: any[]): boolean {
  for (const event of events) {
    if (event.entity_name === entityName) {
      return true;
    }
  }

  return false;
}


export const nodejson = [
  {
    name: '100node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '101node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '102node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '103node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '104node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '105node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '106node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '107node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '108node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '109node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '100node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '101node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '102node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '103node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '104node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '105node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '106node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '107node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '108node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '109node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '100node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '101node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '102node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '103node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '104node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '105node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '106node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '107node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '108node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '109node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '100node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '101node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '102node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '103node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '104node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '105node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '106node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '107node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '108node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '109node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '100node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '101node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '102node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '103node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '104node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '105node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '106node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '107node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '108node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '109node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '100node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '101node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '102node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '103node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '104node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '105node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '106node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '107node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '108node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '109node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '100node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '101node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '102node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '103node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '104node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '105node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '106node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '107node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '108node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '109node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '100node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '101node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '102node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '103node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '104node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '105node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '106node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '107node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '108node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '109node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '100node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '101node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '102node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '103node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '104node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '105node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '106node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '107node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '108node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '109node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '100node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '101node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '102node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '103node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '104node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '105node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '106node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '107node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '108node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '109node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },

  {
    name: '100node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '101node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '102node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '103node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '104node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '105node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '106node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '107node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '108node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  },
  {
    name: '109node-0.0.9.9',
    manager: 'automate',
    target_config: {
      backend: 'ssh',
      secrets: [],
      port: 22,
      sudo: false,
      hosts: ['0.0.9.9']
    },
    tags: []
  }
];


