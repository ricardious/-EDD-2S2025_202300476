unit User;

{$mode ObjFPC}{$H+}

interface

uses
  DoublyLinkedList, Stack, Queue, CircularLinkedList, AVLTree, BTree;

type
  PUser = ^TUser;

  TUser = record
    Id: longint;
    Name: ansistring;
    Username: ansistring;
    Password: ansistring;
    Email: ansistring;
    Phone: ansistring;
    Age: ansistring;
    Inbox: TDoublyLinkedList;
    Trash: TStack;
    ScheduledMail: TQueue;
    Contacts: TCircularLinkedList;
    Drafts: TAVLTree;
    Favorites: TBTree;
  end;

implementation

end.
