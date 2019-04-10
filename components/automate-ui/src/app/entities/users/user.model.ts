export interface User {
  id: string;
  name: string;
  username: string;
  password?: string; // only used for updating, never returned by the API
}

export interface HashMapOfUsers {
  [key: string]: User;
}

export function userHashToArray(userHash: HashMapOfUsers): User[] {
  const userArray: User[] = [];
  Object.values(userHash).forEach((user: User, _) => {
    userArray.push(user);
  });
  return userArray;
}

export function userArrayToHash(userArray: User[]): HashMapOfUsers {
  const userHash: HashMapOfUsers = {};
  userArray.forEach((user: User) => {
    userHash[user.id] = user;
  });
  return userHash;
}
