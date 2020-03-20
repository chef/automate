
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
